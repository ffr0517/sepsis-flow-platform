import { BASELINE_FIELDS, DAY2_FIELDS, STRATA_COUNTRIES, WFAZ_AUTOFILL_SOURCE_KEYS, defaultDay1FormValues } from "./constants/fields.js";
import { buildSummary48h } from "./services/summary48h.js";
import { ApiClient } from "./services/apiClient.js";
import { createConnectionManager, connectionStore } from "./services/connectionManager.js";
import { authStore, setAuthError, setGuestMode } from "./state/authStore.js";
import { settingsStore } from "./state/settingsStore.js";
import { assessmentStore, resetAssessmentState } from "./state/assessmentStore.js";
import { patientStore } from "./state/patientStore.js";
import { workspaceStore } from "./state/workspaceStore.js";
import { cryptoStore } from "./state/cryptoStore.js";
import { guestRepository } from "./data/guestRepository.js";
import { createWorkspaceRepository } from "./data/workspaceRepository.js";
import { createDataAccessFacade } from "./data/dataAccessFacade.js";
import { createAuthService } from "./services/authService.js";
import { byId, escapeHtml } from "./utils/dom.js";
import { asPercent, formatDateTime, hasValue } from "./utils/format.js";
import { loadPersistedSettings, persistSettings, resolveOrchestratorBaseUrl } from "./services/environmentService.js";
import { importGuestDataIntoWorkspace, markGuestImportDecision, shouldPromptGuestImport } from "./services/guestImportService.js";
import { setPendingWorkspaceNameForEmail } from "./services/workspaceService.js";
import { createWorkspaceCryptoService } from "./services/workspaceCryptoService.js";

const runtimeConfig = {
  apiBaseUrls: window.SEPSIS_FLOW_API_BASE_URLS || {
    orchestrator: "https://sepsis-flow-orchestrator.onrender.com",
    day1: "https://sepsis-flow-d1-api.onrender.com",
    day2: "https://sepsis-flow-platform.onrender.com"
  },
  appConfig: window.SEPSIS_FLOW_APP_CONFIG || {},
  supabase: window.SEPSIS_FLOW_SUPABASE || {
    url: "",
    anonKey: ""
  }
};

const runtimeEnvDefaults = {
  prod: runtimeConfig.apiBaseUrls.orchestrator,
  local: runtimeConfig.apiBaseUrls.orchestrator,
  staging: runtimeConfig.appConfig.stagingOrchestratorBaseUrl || ""
};

const uiState = {
  currentPage: "patients",
  statusText: "Initializing application...",
  assessPatientId: null,
  selectedAssessmentId: null,
  connectionGateVisible: true,
  workspaceCryptoPromptVisible: false,
  workspaceCryptoPromptMode: "unlock",
  guestImportPromptVisible: false,
  guestImportResult: null,
  supportExportRedactExternalIds: false,
  supportExportScope: "selected",
  profile: null,
  loading: {
    day1: false,
    day2: false,
    patients: false,
    import: false,
    profile: false
  }
};

function defaultSettings() {
  return {
    environmentSelection: runtimeConfig.appConfig.defaultEnvironment || (runtimeConfig.appConfig.skipStartupWarmup ? "local" : "prod"),
    customStagingBaseUrl: "",
    privacyShowExternalIdByDefault: false,
    skipStartupWarmup: Boolean(runtimeConfig.appConfig.skipStartupWarmup)
  };
}

function readSettings() {
  return settingsStore.getState();
}

function currentBaseUrl() {
  return resolveOrchestratorBaseUrl(readSettings(), runtimeEnvDefaults);
}

function isWorkspaceClientEncryptionEnabled() {
  const config = runtimeConfig.appConfig || {};
  if (config.workspace_client_encryption_v1 === false) return false;
  if (config.workspaceClientEncryptionV1 === false) return false;
  if (config.workspace_client_encryption_v1 === true) return true;
  if (config.workspaceClientEncryptionV1 === true) return true;
  return true;
}

const apiClient = new ApiClient({
  getBaseUrl: () => currentBaseUrl()
});

const connectionManager = createConnectionManager(apiClient, {
  skipWarmup: Boolean(runtimeConfig.appConfig.skipStartupWarmup),
  getWakeUrls: () => ({
    orchestrator: currentBaseUrl(),
    day1: runtimeConfig.apiBaseUrls.day1 || runtimeConfig.apiBaseUrls.day1Api || "https://sepsis-flow-d1-api.onrender.com",
    day2: runtimeConfig.apiBaseUrls.day2 || runtimeConfig.apiBaseUrls.day2Api || "https://sepsis-flow-platform.onrender.com"
  })
});

const authService = createAuthService({
  supabaseUrl: runtimeConfig.supabase.url,
  supabaseAnonKey: runtimeConfig.supabase.anonKey
});

let workspaceCryptoService = null;

const workspaceRepository = createWorkspaceRepository(
  () => authService.getSupabase(),
  () => authStore.getState(),
  () => workspaceStore.getState().workspace,
  () => workspaceCryptoService?.getCryptoContext?.() || { enabled: false, key: null, workspaceId: null },
  () => workspaceStore.getState().membershipRole
);

const dataAccess = createDataAccessFacade({
  getMode: () => authStore.getState().mode,
  guestRepository,
  workspaceRepository
});

workspaceCryptoService = createWorkspaceCryptoService({
  dataAccess,
  getAuthState: () => authStore.getState(),
  getWorkspaceState: () => workspaceStore.getState(),
  featureEnabled: () => isWorkspaceClientEncryptionEnabled()
});

function modeLabel() {
  const auth = authStore.getState();
  if (auth.mode !== "authenticated") return "Guest (local only)";
  const workspace = workspaceStore.getState();
  const role = workspace.membershipRole ? `, ${workspace.membershipRole}` : "";
  return `Workspace${role}`;
}

function setStatus(text) {
  uiState.statusText = text;
  renderGlobalStatus();
}

function requiresWorkspaceUnlock() {
  return isWorkspaceClientEncryptionEnabled() && authStore.getState().mode === "authenticated";
}

function isWorkspaceUnlocked() {
  if (!requiresWorkspaceUnlock()) return true;
  return cryptoStore.getState().state === "unlocked";
}

function ensureWorkspaceUnlocked() {
  if (!isWorkspaceUnlocked()) {
    throw new Error("Unlock workspace with passphrase before accessing workspace data.");
  }
}

function ensureConnectionReady() {
  if (!connectionManager.isReady()) {
    throw new Error("APIs are not ready yet. Click 'Check API Status' first.");
  }
}

function getAssessPatient() {
  const id = uiState.assessPatientId;
  if (!id) return null;
  return patientStore.getState().patients.find((row) => row.id === id) || null;
}

function parseNullableNumber(value) {
  if (value === null || value === undefined || String(value).trim() === "") return null;
  const n = Number(value);
  return Number.isFinite(n) ? n : null;
}

function parseSexValue(value) {
  const n = parseNullableNumber(value);
  if (n === null) return null;
  return n > 0.5 ? 1 : 0;
}

function parseWeightUnitValue(value) {
  if (value === null || value === undefined || String(value).trim() === "") return null;
  const text = String(value).trim().toLowerCase();
  if (text === "kg" || text === "1") return "kg";
  if (text === "lbs" || text === "lb" || text === "0") return "lbs";
  return null;
}

function parseInpatientStatus(value) {
  if (value === null || value === undefined || String(value).trim() === "") return null;
  const text = String(value).trim().toLowerCase();
  if (text === "inpatient") return "Inpatient";
  if (text === "outpatient") return "Outpatient";
  return null;
}

function normalizeProfilePayload(input = {}) {
  return {
    country: hasValue(input.country) ? String(input.country).trim() : null,
    inpatientStatus: parseInpatientStatus(input.inpatientStatus),
    ageMonths: parseNullableNumber(input.ageMonths),
    sex: parseSexValue(input.sex),
    weightValue: parseNullableNumber(input.weightValue),
    weightUnit: parseWeightUnitValue(input.weightUnit)
  };
}

function readProfileFromCreatePatientForm(formData) {
  return normalizeProfilePayload({
    country: formData.get("country"),
    inpatientStatus: formData.get("inpatientStatus"),
    ageMonths: formData.get("ageMonths"),
    sex: formData.get("sex"),
    weightValue: formData.get("weightValue"),
    weightUnit: formData.get("weightUnit")
  });
}

function valuesEqual(a, b) {
  if (a === null || a === undefined || a === "") return b === null || b === undefined || b === "";
  if (b === null || b === undefined || b === "") return false;
  return String(a) === String(b);
}

function patientProfileDiffers(patient, nextProfile) {
  if (!patient) return false;
  return !(
    valuesEqual(patient.country, nextProfile.country) &&
    valuesEqual(patient.inpatientStatus, nextProfile.inpatientStatus) &&
    valuesEqual(patient.ageMonths, nextProfile.ageMonths) &&
    valuesEqual(patient.sex, nextProfile.sex) &&
    valuesEqual(patient.weightValue, nextProfile.weightValue) &&
    valuesEqual(patient.weightUnit, nextProfile.weightUnit)
  );
}

function profileFromAssessmentInputs() {
  const sex = Number(document.querySelector('input[name="sex"]:checked')?.value);
  const weightUnitCode = Number(document.querySelector('input[name="weight.unit"]:checked')?.value);

  return normalizeProfilePayload({
    country: byId("priorCountry")?.value,
    inpatientStatus: byId("priorInpatientStatus")?.value,
    ageMonths: byId("age.months")?.value,
    sex: Number.isFinite(sex) ? sex : null,
    weightValue: byId("weight.value")?.value,
    weightUnit: Number.isFinite(weightUnitCode) ? (weightUnitCode === 1 ? "kg" : "lbs") : null
  });
}

function buildDay1DefaultsForPatient(patient) {
  const base = defaultDay1FormValues();
  if (!patient) return base;

  const profile = normalizeProfilePayload(patient);
  const next = { ...base };

  if (profile.ageMonths !== null) next["age.months"] = profile.ageMonths;
  if (profile.sex !== null) next.sex = profile.sex;
  if (profile.weightValue !== null) next["weight.value"] = profile.weightValue;
  if (profile.weightUnit) next["weight.unit"] = profile.weightUnit === "kg" ? 1 : 0;

  return next;
}

function setAssessStrataFromPatient(patient) {
  const profile = normalizeProfilePayload(patient || {});
  byId("priorCountry").value = profile.country || "";
  byId("priorInpatientStatus").value = profile.inpatientStatus || "";
}

function clearAssessStrata() {
  byId("priorCountry").value = "";
  byId("priorInpatientStatus").value = "";
}

function ensureAssessStateForPatient(patientId) {
  resetAssessmentState();
  const draft = {
    id: null,
    patientId,
    status: "day1_complete",
    environment: readSettings().environmentSelection,
    orchestratorBaseUrl: currentBaseUrl(),
    baselineInputs: null,
    day1Outputs: null,
    day2CarryForwardEdited: null,
    day2Outputs: null,
    strata: null,
    summary48h: null,
    modelMetadata: null,
    createdAt: null
  };
  assessmentStore.patch({ draft });
  renderDay2Form({});
}

function selectAssessPatient(patientId, { preserveResults = false } = {}) {
  uiState.assessPatientId = patientId || null;

  const patient = getAssessPatient();
  if (!preserveResults) {
    if (patient) {
      ensureAssessStateForPatient(patient.id);
      renderDay1Form(buildDay1DefaultsForPatient(patient));
      setAssessStrataFromPatient(patient);
    } else {
      resetAssessmentState();
      renderDay1Form(defaultDay1FormValues());
      renderDay2Form({});
      clearAssessStrata();
    }
  }

  renderAssessState();
}

function persistSettingsToLocal() {
  const state = readSettings();
  persistSettings(state);
  if (authStore.getState().mode === "authenticated") {
    dataAccess.upsertAppSetting("frontend_preferences", {
      privacyShowExternalIdByDefault: state.privacyShowExternalIdByDefault
    }).catch(() => {
      // keep app resilient if workspace settings write fails
    });
  }
}

async function refreshPatients() {
  uiState.loading.patients = true;
  renderPatientsState();

  try {
    if (requiresWorkspaceUnlock() && !isWorkspaceUnlocked()) {
      patientStore.patch({
        patients: [],
        filteredPatients: [],
        selectedPatient: null,
        selectedPatientId: null,
        selectedPatientAssessments: [],
        error: "Workspace is locked. Unlock with passphrase to view patient data.",
        loading: false
      });
      return;
    }

    const { search, selectedPatientId } = patientStore.getState();
    const patients = await dataAccess.listPatients(search);
    const selected = patients.find((row) => row.id === selectedPatientId) || null;

    patientStore.patch({
      patients,
      filteredPatients: patients,
      selectedPatient: selected,
      error: null,
      loading: false
    });

    if (selectedPatientId && selected) {
      await refreshTimeline(selectedPatientId);
    } else {
      patientStore.patch({ selectedPatientAssessments: [] });
    }

    if (uiState.assessPatientId && !patients.some((row) => row.id === uiState.assessPatientId)) {
      selectAssessPatient(null);
    }
  } catch (error) {
    patientStore.patch({ error: error?.message || "Failed to load patients.", loading: false });
  } finally {
    uiState.loading.patients = false;
    renderPatientsState();
    renderAssessState();
  }
}

async function refreshTimeline(patientId) {
  if (!patientId) {
    patientStore.patch({ selectedPatientAssessments: [] });
    return;
  }

  try {
    const rows = await dataAccess.listAssessmentsByPatient(patientId);
    patientStore.patch({ selectedPatientAssessments: rows, error: null });
  } catch (error) {
    patientStore.patch({ error: error?.message || "Failed to load timeline." });
  }

  renderPatientsState();
}

function readNumberInput(id) {
  const el = byId(id);
  if (el && el.type !== "radio") {
    const raw = String(el.value || "").trim();
    if (!raw) throw new Error(`Missing value for ${id}.`);
    const num = Number(raw);
    if (!Number.isFinite(num)) throw new Error(`Invalid value for ${id}.`);
    return num;
  }

  const radios = document.getElementsByName(id);
  if (radios?.length) {
    for (const radio of radios) {
      if (radio.checked) {
        const n = Number(radio.value);
        if (!Number.isFinite(n)) throw new Error(`Invalid value for ${id}.`);
        return n;
      }
    }
  }

  throw new Error(`Missing value for ${id}.`);
}

function collectBaselineInputs() {
  const out = {};
  for (const field of BASELINE_FIELDS) {
    if (field.clientOnly) continue;
    out[field.key] = readNumberInput(field.key);
  }
  return out;
}

function collectDay2Prefill() {
  const out = {};
  for (const field of DAY2_FIELDS) {
    out[field.key] = readNumberInput(field.key) > 0.5 ? 1 : 0;
  }
  return out;
}

function collectStrata() {
  const country = String(byId("priorCountry")?.value || "").trim();
  const inpatientStatus = String(byId("priorInpatientStatus")?.value || "").trim();
  const strata = {};
  if (country) strata.country = country;
  if (inpatientStatus) strata.inpatient_status = inpatientStatus;
  return strata;
}

function withOptionalStrata(payload, strata) {
  if (!strata || Object.keys(strata).length === 0) return payload;
  return { ...payload, strata };
}

function formatPredictionRow(row) {
  return {
    treatment: row.level,
    predicted: Boolean(row.predicted_treatment_by_majority_vote),
    probabilityPct: hasValue(row.p_adj) ? asPercent(row.p_adj) : asPercent(row.mean_predicted_probability),
    thresholdPct: hasValue(row.t_adj) ? asPercent(row.t_adj) : asPercent(row.t_50_50),
    votersExceedingThreshold: row.votes_exceeding_threshold,
    votesAboveThresholdPct: asPercent(row.votes_above_threshold),
    prevalenceScope: row.prevalence_scope || "",
    prevalenceStratum: row.prevalence_stratum || ""
  };
}

function predictionCardsHtml(rows = []) {
  if (!rows.length) return "<p class='hint'>No predictions returned.</p>";

  const formatted = rows.map(formatPredictionRow);
  const predicted = formatted.filter((row) => row.predicted);
  const others = formatted.filter((row) => !row.predicted);

  const card = (row, predictedCard = false) => `
    <article class="prediction-card ${predictedCard ? "predicted" : ""}">
      <h4>${escapeHtml(row.treatment)}</h4>
      <p><strong>Probability:</strong> ${escapeHtml(row.probabilityPct)}%</p>
      <p><strong>Threshold:</strong> ${escapeHtml(row.thresholdPct)}%</p>
      <p><strong>Voters above threshold:</strong> ${escapeHtml(row.votersExceedingThreshold)}</p>
      <p><strong>Vote share:</strong> ${escapeHtml(row.votesAboveThresholdPct)}%</p>
      ${row.prevalenceScope ? `<p><strong>Prevalence stratum:</strong> ${escapeHtml(row.prevalenceScope)} / ${escapeHtml(row.prevalenceStratum)}</p>` : ""}
    </article>
  `;

  return `
    ${predicted.length
      ? `<section class="prediction-section">
          <h3>Predicted treatments</h3>
          <div class="prediction-list">${predicted.map((row) => card(row, true)).join("")}</div>
        </section>`
      : "<p class='hint'>No treatments predicted by majority vote.</p>"}

    ${others.length
      ? `<details class="prediction-collapsed">
          <summary>Other treatments (${others.length})</summary>
          <div class="prediction-list">${others.map((row) => card(row, false)).join("")}</div>
        </details>`
      : ""}
  `;
}

function predictedTreatmentsList(rows = []) {
  return rows
    .filter((row) => row?.predicted_treatment_by_majority_vote)
    .map((row) => row.level);
}

function profileSummaryHtml(patient) {
  const p = normalizeProfilePayload(patient || {});
  const sexLabel = p.sex === 1 ? "Male" : p.sex === 0 ? "Female" : "Not set";
  return `
    <div class="profile-grid">
      <div><strong>External ID:</strong> ${escapeHtml(patient?.externalId || "Not set")}</div>
      <div><strong>Country:</strong> ${escapeHtml(p.country || "Not set")}</div>
      <div><strong>Inpatient status:</strong> ${escapeHtml(p.inpatientStatus || "Not set")}</div>
      <div><strong>Age (months):</strong> ${p.ageMonths ?? "Not set"}</div>
      <div><strong>Sex:</strong> ${sexLabel}</div>
      <div><strong>Weight:</strong> ${p.weightValue ?? "Not set"} ${p.weightUnit || ""}</div>
    </div>
  `;
}

async function ensureWfazReadyForDay1() {
  const wfazInput = byId("wfaz");
  const ageMonths = Number(byId("age.months")?.value);
  const sex = Number(document.querySelector('input[name="sex"]:checked')?.value);
  const weight = Number(byId("weight.value")?.value);
  const weightUnit = Number(document.querySelector('input[name="weight.unit"]:checked')?.value);

  if (![ageMonths, sex, weight, weightUnit].every(Number.isFinite)) {
    throw new Error("Enter sex, age, weight and weight unit to auto-calculate WFAZ.");
  }

  const envelope = await apiClient.calculateWfaz({
    sex,
    "age.months": ageMonths,
    weight,
    weight_unit: weightUnit === 1 ? "kg" : "lbs"
  });

  const wfaz = Number(envelope?.data?.wfaz);
  if (!Number.isFinite(wfaz)) {
    throw new Error("WFAZ endpoint returned an invalid value.");
  }
  wfazInput.value = wfaz.toFixed(2);
}

async function maybePromptUpdatePatientProfileFromAssessment(patientId) {
  const patient = patientStore.getState().patients.find((row) => row.id === patientId) || null;
  if (!patient) return;

  const fromAssessment = profileFromAssessmentInputs();
  if (!patientProfileDiffers(patient, fromAssessment)) return;

  const shouldUpdate = window.confirm("Update this patient's default profile values from the current assessment inputs?");
  if (!shouldUpdate) return;

  await dataAccess.updatePatient(patientId, fromAssessment);
  await refreshPatients();
  if (patientStore.getState().selectedPatientId === patientId) {
    await refreshTimeline(patientId);
  }
}

async function runDay1() {
  ensureWorkspaceUnlocked();
  ensureConnectionReady();
  const patientId = uiState.assessPatientId;
  if (!patientId) throw new Error("Select a patient before running Day 1.");

  uiState.loading.day1 = true;
  renderAssessState();

  try {
    await ensureWfazReadyForDay1();
    const baselineInputs = collectBaselineInputs();
    const strata = collectStrata();

    const envelope = await apiClient.flowDay1(withOptionalStrata({ data: baselineInputs }, strata));
    const day1Rows = envelope?.data?.day1_result || [];
    const prefill = envelope?.data?.day2_prefill || {};

    const draft = {
      ...(assessmentStore.getState().draft || {}),
      patientId,
      status: "day1_complete",
      environment: readSettings().environmentSelection,
      orchestratorBaseUrl: currentBaseUrl(),
      baselineInputs,
      day1Outputs: day1Rows,
      day2CarryForwardEdited: prefill,
      day2Outputs: null,
      strata,
      summary48h: buildSummary48h(day1Rows, []),
      modelMetadata: {
        healthSnapshotAt: connectionStore.getState().lastCheckedAt,
        health: connectionStore.getState().lastHealth,
        day1Trace: envelope?.trace || null,
        day2Trace: null,
        version: "v1"
      }
    };

    const saved = await dataAccess.upsertAssessment(draft);

    assessmentStore.patch({
      draft: { ...draft, id: saved.id, createdAt: saved.createdAt },
      baselineInputs,
      priorAdjustments: strata,
      day1Response: envelope,
      day2Response: null,
      day2Prefill: prefill,
      editedDay2CarryForward: prefill,
      lastSavedAssessmentId: saved.id,
      unsavedBannerVisible: false
    });

    renderDay2Form(prefill);
    await maybePromptUpdatePatientProfileFromAssessment(patientId);
    await refreshPatients();
    if (patientStore.getState().selectedPatientId === patientId) {
      await refreshTimeline(patientId);
    }

    setStatus("Day 1 complete and saved to patient timeline.");
  } catch (error) {
    setStatus(error?.message || "Day 1 failed.");
    throw error;
  } finally {
    uiState.loading.day1 = false;
    renderAssessState();
  }
}

async function runDay2() {
  ensureWorkspaceUnlocked();
  ensureConnectionReady();
  const patientId = uiState.assessPatientId;
  if (!patientId) throw new Error("Select a patient before running Day 2.");

  const state = assessmentStore.getState();
  if (!state.baselineInputs) throw new Error("Run Day 1 first.");

  uiState.loading.day2 = true;
  renderAssessState();

  try {
    const day2Prefill = collectDay2Prefill();
    const strata = collectStrata();

    const envelope = await apiClient.flowDay2(withOptionalStrata({
      baseline_inputs: state.baselineInputs,
      day2_prefill: day2Prefill
    }, strata));

    const day1Rows = state.day1Response?.data?.day1_result || [];
    const day2Rows = envelope?.data?.day2_result || [];

    const draft = {
      ...(state.draft || {}),
      patientId,
      status: "day2_complete",
      day2CarryForwardEdited: day2Prefill,
      day2Outputs: day2Rows,
      strata,
      summary48h: buildSummary48h(day1Rows, day2Rows),
      modelMetadata: {
        ...(state.draft?.modelMetadata || {}),
        day2Trace: envelope?.trace || null
      }
    };

    const saved = await dataAccess.upsertAssessment(draft);

    assessmentStore.patch({
      draft: { ...draft, id: saved.id, createdAt: saved.createdAt },
      day2Response: envelope,
      day2Prefill,
      editedDay2CarryForward: day2Prefill,
      priorAdjustments: strata,
      lastSavedAssessmentId: saved.id
    });

    await refreshPatients();
    if (patientStore.getState().selectedPatientId === patientId) {
      await refreshTimeline(patientId);
    }

    setStatus("Day 2 complete and saved to patient timeline.");
  } catch (error) {
    setStatus(error?.message || "Day 2 failed.");
    throw error;
  } finally {
    uiState.loading.day2 = false;
    renderAssessState();
  }
}

function csvEscape(value) {
  const str = String(value ?? "");
  if (/[",\n]/.test(str)) return `"${str.replace(/"/g, '""')}"`;
  return str;
}

function buildCsv(patients, assessments) {
  const header = [
    "Patient Alias",
    "Patient External ID",
    "Country",
    "Inpatient Status",
    "Age Months",
    "Sex",
    "Weight Value",
    "Weight Unit",
    "Assessment ID",
    "Status",
    "Environment",
    "Created At",
    "Updated At",
    "48h Highest Level",
    "48h Rationale"
  ];

  const patientById = new Map((patients || []).map((row) => [row.id, row]));
  const lines = [header.map(csvEscape).join(",")];

  for (const row of assessments || []) {
    const patient = patientById.get(row.patientId);
    lines.push([
      patient?.alias || "",
      patient?.externalId || "",
      patient?.country || "",
      patient?.inpatientStatus || "",
      patient?.ageMonths ?? "",
      patient?.sex ?? "",
      patient?.weightValue ?? "",
      patient?.weightUnit || "",
      row.id,
      row.status,
      row.environment,
      row.createdAt,
      row.updatedAt,
      row.summary48h?.highestLevelLabel || "",
      row.summary48h?.rationale || ""
    ].map(csvEscape).join(","));
  }

  return lines.join("\n");
}

function buildSupportPackage({ patients, assessments, scope, redactExternalIds }) {
  const safePatients = (patients || []).map((row) => ({
    ...row,
    externalId: redactExternalIds ? null : (row.externalId || null)
  }));

  return {
    exportedAt: new Date().toISOString(),
    scope,
    redactExternalIds: Boolean(redactExternalIds),
    mode: authStore.getState().mode,
    workspace: workspaceStore.getState().workspace || null,
    environment: readSettings().environmentSelection,
    orchestratorBaseUrl: currentBaseUrl(),
    appVersion: runtimeConfig.appConfig.appVersion || "workspace-web-v1",
    patients: safePatients,
    assessments: assessments || []
  };
}

function downloadText(filename, text, mime = "text/plain;charset=utf-8") {
  const blob = new Blob([text], { type: mime });
  const url = URL.createObjectURL(blob);
  const a = document.createElement("a");
  a.href = url;
  a.download = filename;
  document.body.appendChild(a);
  a.click();
  a.remove();
  URL.revokeObjectURL(url);
}

function renderGlobalStatus() {
  const dot = byId("sidebarStatusDot");
  const text = byId("sidebarStatusText");
  const mode = byId("sidebarModeBadge");
  const conn = connectionStore.getState();

  if (!dot || !text || !mode) return;

  dot.className = `status-dot status-${conn.state}`;
  text.textContent = uiState.statusText || conn.message;
  mode.textContent = modeLabel();
}

function renderWarmupStatusCard() {
  const card = byId("warmupCard");
  const chip = byId("warmupChip");
  const text = byId("warmupText");
  const actionBtn = byId("retryWarmupBtn");
  const localSkip = Boolean(readSettings().skipStartupWarmup);
  const conn = connectionStore.getState();

  if (card) card.classList.toggle("hidden", localSkip);
  if (localSkip) return;

  if (chip) {
    chip.textContent = conn.warmupChipLabel || "Pending";
    chip.className = `chip ${conn.warmupChipClass || ""}`.trim();
  }
  if (text) {
    text.textContent = conn.warmupText
      || "Manual check only. Click 'Check API Status' to send wake-up requests to the orchestrator, Day 1 API, and Day 2 API.";
  }
  if (actionBtn) {
    actionBtn.disabled = Boolean(conn.warming);
    actionBtn.textContent = conn.warming ? "Checking..." : "Check API Status";
  }
}

function setAssessInteractionLocked(locked) {
  const cards = document.querySelectorAll(".warmup-gateable");
  cards.forEach((card) => {
    card.classList.toggle("locked", locked);
    card.querySelectorAll("input, button, select, textarea").forEach((el) => {
      if (!(el instanceof HTMLInputElement || el instanceof HTMLButtonElement || el instanceof HTMLSelectElement || el instanceof HTMLTextAreaElement)) {
        return;
      }
      el.disabled = locked;
    });
  });
}

function renderNav() {
  const pages = ["new-patient", "patients", "assess", "settings", "profile"];
  for (const page of pages) {
    const btn = byId(`nav-${page}`);
    const section = byId(`page-${page}`);
    if (!btn || !section) continue;
    btn.classList.toggle("active", uiState.currentPage === page);
    section.classList.toggle("hidden", uiState.currentPage !== page);
  }
}

function renderPatientsState() {
  const state = patientStore.getState();
  const listEl = byId("patientsList");
  const detailsEl = byId("patientDetailsPanel");
  const timelineEl = byId("timelineList");

  if (!listEl || !detailsEl || !timelineEl) return;

  byId("patientsError").textContent = state.error || "";

  if (!state.filteredPatients.length) {
    listEl.innerHTML = "<p class='hint'>No patients found.</p>";
  } else {
    const showExternal = readSettings().privacyShowExternalIdByDefault;
    listEl.innerHTML = `
      <table class="list-table">
        <thead>
          <tr>
            <th>Alias</th>
            <th>External ID</th>
            <th>Last Assessment</th>
            <th></th>
          </tr>
        </thead>
        <tbody>
          ${state.filteredPatients.map((row) => `
            <tr class="${row.id === state.selectedPatientId ? "selected" : ""}">
              <td>${escapeHtml(row.alias)}</td>
              <td>${showExternal ? escapeHtml(row.externalId || "") : "Hidden"}</td>
              <td>${escapeHtml(formatDateTime(row.lastAssessmentAt))}</td>
              <td>
                <button data-action="select-patient" data-patient-id="${row.id}" class="btn btn-small">Open</button>
                <button data-action="delete-patient" data-patient-id="${row.id}" class="btn btn-small btn-danger">Delete</button>
              </td>
            </tr>
          `).join("")}
        </tbody>
      </table>
    `;
  }

  byId("patientsModeBadge").textContent = modeLabel();

  if (!state.selectedPatient) {
    detailsEl.innerHTML = "<p class='hint'>Open a patient to view details and latest predictions.</p>";
    timelineEl.innerHTML = "<p class='hint'>Select a patient to view timeline.</p>";
    return;
  }

  const latest = state.selectedPatientAssessments[0] || null;
  const latestDay1Predicted = latest ? predictedTreatmentsList(latest.day1Outputs || []) : [];
  const latestDay2Predicted = latest ? predictedTreatmentsList(latest.day2Outputs || []) : [];
  const selectedSex = state.selectedPatient.sex === 1 ? "Male" : state.selectedPatient.sex === 0 ? "Female" : "";
  const selectedWeightUnit = state.selectedPatient.weightUnit || "";

  detailsEl.innerHTML = `
    <div class="patient-details-header">
      <div>
        <h3>${escapeHtml(state.selectedPatient.alias)}</h3>
        <p class="muted">Created ${escapeHtml(formatDateTime(state.selectedPatient.createdAt))} • Updated ${escapeHtml(formatDateTime(state.selectedPatient.updatedAt))}</p>
      </div>
      <div class="actions-row">
        <button data-action="open-in-assess" data-patient-id="${state.selectedPatient.id}" class="btn btn-small btn-primary">Open in Assess</button>
        <button data-action="clear-selected-patient" class="btn btn-small">Clear</button>
      </div>
    </div>

    ${profileSummaryHtml(state.selectedPatient)}

    <form id="editPatientForm" class="mini-form edit-patient-form">
      <input type="hidden" name="patientId" value="${escapeHtml(state.selectedPatient.id)}" />
      <h4>Edit patient profile</h4>
      <div class="grid-two">
        <input name="alias" value="${escapeHtml(state.selectedPatient.alias || "")}" placeholder="Alias" required />
        <input name="externalId" value="${escapeHtml(state.selectedPatient.externalId || "")}" placeholder="External ID (optional)" />
      </div>
      <div class="grid-two">
        <select name="country">
          <option value="">Country (optional)</option>
          ${STRATA_COUNTRIES.map((country) => `<option value="${escapeHtml(country)}" ${country === state.selectedPatient.country ? "selected" : ""}>${escapeHtml(country)}</option>`).join("")}
        </select>
        <select name="inpatientStatus">
          <option value="">Inpatient status (optional)</option>
          <option value="Inpatient" ${state.selectedPatient.inpatientStatus === "Inpatient" ? "selected" : ""}>Inpatient</option>
          <option value="Outpatient" ${state.selectedPatient.inpatientStatus === "Outpatient" ? "selected" : ""}>Outpatient</option>
        </select>
      </div>
      <div class="grid-three">
        <input name="ageMonths" type="number" min="0" step="1" value="${escapeHtml(state.selectedPatient.ageMonths ?? "")}" placeholder="Age (months)" />
        <select name="sex">
          <option value="" ${selectedSex ? "" : "selected"}>Sex</option>
          <option value="1" ${selectedSex === "Male" ? "selected" : ""}>Male</option>
          <option value="0" ${selectedSex === "Female" ? "selected" : ""}>Female</option>
        </select>
        <input name="weightValue" type="number" min="0" step="0.01" value="${escapeHtml(state.selectedPatient.weightValue ?? "")}" placeholder="Weight value" />
      </div>
      <div class="grid-two">
        <select name="weightUnit">
          <option value="" ${selectedWeightUnit ? "" : "selected"}>Weight unit</option>
          <option value="kg" ${selectedWeightUnit === "kg" ? "selected" : ""}>kg</option>
          <option value="lbs" ${selectedWeightUnit === "lbs" ? "selected" : ""}>lbs</option>
        </select>
        <button class="btn" type="submit">Save changes</button>
      </div>
    </form>

    <div class="latest-assessment-box">
      <h4>Latest assessment snapshot</h4>
      ${latest
        ? `
          <p><strong>When:</strong> ${escapeHtml(formatDateTime(latest.createdAt))}</p>
          <p><strong>Status:</strong> ${escapeHtml(latest.status)}</p>
          <p><strong>Day 1 predicted:</strong> ${latestDay1Predicted.length ? escapeHtml(latestDay1Predicted.join(", ")) : "None"}</p>
          <p><strong>Day 2 predicted:</strong> ${latestDay2Predicted.length ? escapeHtml(latestDay2Predicted.join(", ")) : "None"}</p>
          <p><strong>48h summary:</strong> ${escapeHtml(latest.summary48h?.rationale || "No summary")}</p>
        `
        : "<p class='hint'>No assessments yet for this patient.</p>"}
    </div>
  `;

  if (!state.selectedPatientAssessments.length) {
    timelineEl.innerHTML = "<p class='hint'>No assessments yet.</p>";
  } else {
    timelineEl.innerHTML = `
      <div class="timeline-cards">
        ${state.selectedPatientAssessments.map((row) => `
          <article class="timeline-card ${row.id === uiState.selectedAssessmentId ? "active" : ""}">
            <header>
              <strong>${escapeHtml(formatDateTime(row.createdAt))}</strong>
              <span class="chip">${escapeHtml(row.status)}</span>
            </header>
            <p><strong>48h summary:</strong> ${escapeHtml(row.summary48h?.rationale || "No summary")}</p>
            <div class="actions-row">
              <button data-action="open-assessment" data-assessment-id="${row.id}" class="btn btn-small">Review</button>
            </div>
          </article>
        `).join("")}
      </div>
    `;
  }
}

function renderAssessState() {
  const assess = assessmentStore.getState();
  const patient = getAssessPatient();
  const conn = connectionStore.getState();
  const ready = connectionManager.isReady();
  const interactionLocked = !ready || conn.warming;

  const selector = byId("assessPatientSelect");
  if (selector) {
    const patients = patientStore.getState().patients || [];
    const current = uiState.assessPatientId || "";
    selector.innerHTML = `<option value="">Select patient</option>${patients.map((row) => `<option value="${row.id}" ${row.id === current ? "selected" : ""}>${escapeHtml(row.alias)}</option>`).join("")}`;
  }

  byId("assessmentContextText").textContent = patient
    ? `Selected patient: ${patient.alias}`
    : "Select a patient to begin assessment.";

  setAssessInteractionLocked(interactionLocked);
  const canRun = ready && Boolean(patient);
  byId("runDay1Btn").disabled = uiState.loading.day1 || !canRun;
  byId("runDay2Btn").disabled = uiState.loading.day2 || !canRun || !assess.baselineInputs;
  byId("exportAssessPatientBtn").disabled = !patient || !ready;

  byId("runDay1Btn").textContent = uiState.loading.day1 ? "Running Day 1..." : "Run Day 1";
  byId("runDay2Btn").textContent = uiState.loading.day2 ? "Running Day 2..." : "Run Day 2";

  byId("day1Results").innerHTML = assess.day1Response
    ? predictionCardsHtml(assess.day1Response?.data?.day1_result || [])
    : "<p class='hint'>Run Day 1 to see results.</p>";

  byId("day2Results").innerHTML = assess.day2Response
    ? predictionCardsHtml(assess.day2Response?.data?.day2_result || [])
    : "<p class='hint'>Run Day 2 to see results.</p>";

  const summary = assess.draft?.summary48h;
  byId("summary48h").innerHTML = summary
    ? `<p><strong>${escapeHtml(summary.rationale)}</strong></p><p>Day 1 predicted: ${summary.day1PredictedCount} | Day 2 predicted: ${summary.day2PredictedCount}</p>`
    : "<p class='hint'>48-hour summary appears after Day 1 or Day 2 run.</p>";
}

function renderSettingsState() {
  const auth = authStore.getState();
  const settings = readSettings();

  const authPanel = byId("authPanel");
  if (auth.mode === "authenticated") {
    authPanel.innerHTML = `
      <div class="auth-summary">
        <p><strong>Signed in:</strong> ${escapeHtml(auth.user?.email || "")}</p>
        <button id="signOutBtn" class="btn">Sign out</button>
      </div>
    `;
  } else {
    authPanel.innerHTML = `
      <div class="auth-forms">
        <form id="signInForm" class="mini-form">
          <h4>Sign in</h4>
          <input type="email" name="email" required placeholder="Email" />
          <input type="password" name="password" required placeholder="Password" />
          <button class="btn btn-primary" type="submit">Sign in</button>
        </form>
        <form id="signUpForm" class="mini-form">
          <h4>Create account</h4>
          <input type="email" name="email" required placeholder="Email" />
          <input type="password" name="password" required placeholder="Password" minlength="8" />
          <input type="text" name="workspaceName" placeholder="Workspace name (optional)" />
          <button class="btn" type="submit">Sign up</button>
        </form>
        <form id="resetPasswordForm" class="mini-form">
          <h4>Reset password</h4>
          <input type="email" name="email" required placeholder="Email" />
          <button class="btn" type="submit">Send reset email</button>
        </form>
        <button id="guestModeBtn" class="btn">Continue as guest</button>
      </div>
    `;
  }

  byId("authErrorText").textContent = auth.error || "";

  byId("runtimeEnvironmentText").textContent = `${settings.environmentSelection.toUpperCase()} (${currentBaseUrl()})`;
  byId("privacyShowExternalIdByDefault").checked = settings.privacyShowExternalIdByDefault;
  const redactBox = byId("supportExportRedactExternalIds");
  if (redactBox) redactBox.checked = Boolean(uiState.supportExportRedactExternalIds);
  const scopeSelect = byId("supportExportScopeSelect");
  if (scopeSelect) scopeSelect.value = uiState.supportExportScope;
  const exportLocked = (requiresWorkspaceUnlock() && !isWorkspaceUnlocked()) || !connectionManager.isReady();
  const supportBtn = byId("exportSupportPackageBtn");
  const allCsvBtn = byId("exportAllCsvBtn");
  const jsonBtn = byId("exportJsonBtn");
  if (supportBtn) supportBtn.disabled = exportLocked;
  if (allCsvBtn) allCsvBtn.disabled = exportLocked;
  if (jsonBtn) jsonBtn.disabled = exportLocked;

  const importPanel = byId("importResultPanel");
  if (uiState.guestImportResult) {
    importPanel.innerHTML = `
      <p>Imported patients: ${uiState.guestImportResult.importedPatients}</p>
      <p>Imported assessments: ${uiState.guestImportResult.importedAssessments}</p>
      <p>Failures: ${uiState.guestImportResult.failures.length}</p>
    `;
  } else {
    importPanel.innerHTML = "";
  }
}

function renderProfileState() {
  const auth = authStore.getState();
  const workspace = workspaceStore.getState();
  const panel = byId("profileMainPanel");
  if (!panel) return;

  if (auth.mode !== "authenticated") {
    panel.innerHTML = "<p class='hint'>Sign in to manage profile and workspace settings.</p>";
    return;
  }

  if (requiresWorkspaceUnlock() && !isWorkspaceUnlocked()) {
    panel.innerHTML = "<p class='hint'>Unlock workspace with passphrase to manage profile, members, and encrypted workspace data.</p>";
    return;
  }

  const profile = uiState.profile || {};
  const members = workspace.members || [];
  const invites = workspace.invites || [];
  const isOwner = workspace.membershipRole === "owner";

  panel.innerHTML = `
    <section class="card">
      <h2>User profile</h2>
      <form id="profileForm" class="mini-form inline-profile">
        <div class="field">
          <label for="displayNameInput">Display name</label>
          <input id="displayNameInput" name="displayName" value="${escapeHtml(profile.display_name || "")}" placeholder="Your name" />
        </div>
        <button class="btn" type="submit">Save profile</button>
      </form>
    </section>

    <section class="card">
      <h2>Workspace</h2>
      <p><strong>Current workspace:</strong> ${escapeHtml(workspace.workspace?.name || "")}</p>
      <p><strong>Your role:</strong> ${escapeHtml(workspace.membershipRole || "")}</p>
      ${isOwner ? `
        <form id="workspaceRenameForm" class="mini-form inline-profile">
          <div class="field">
            <label for="workspaceNameInput">Rename workspace</label>
            <input id="workspaceNameInput" name="workspaceName" value="${escapeHtml(workspace.workspace?.name || "")}" required />
          </div>
          <button class="btn" type="submit">Save workspace name</button>
        </form>
        <div class="danger-box">
          <p class="small"><strong>Reset encrypted workspace data:</strong> removes all workspace patients and assessments. This cannot be undone.</p>
          <button id="resetEncryptedWorkspaceBtn" class="btn btn-danger" type="button">Reset encrypted workspace data</button>
        </div>
      ` : "<p class='hint'>Only owners can rename workspace.</p>"}
    </section>

    <section class="card">
      <h2>Members and invites</h2>
      ${isOwner ? `
        <form id="profileInviteForm" class="mini-form inline-profile">
          <div class="field">
            <label for="inviteEmailInput">Invite member by email</label>
            <input id="inviteEmailInput" name="email" type="email" required placeholder="member@example.com" />
          </div>
          <button class="btn" type="submit">Send invite</button>
        </form>
      ` : "<p class='hint'>Only owners can send invites.</p>"}
      <div class="workspace-columns">
        <div>
          <h4>Members</h4>
          ${members.length
            ? `<ul>${members.map((m) => `<li>${escapeHtml(m.user_id)} (${escapeHtml(m.role)})</li>`).join("")}</ul>`
            : "<p class='hint'>No members found.</p>"}
        </div>
        <div>
          <h4>Invites</h4>
          ${invites.length
            ? `<ul>${invites.map((i) => `<li>${escapeHtml(i.email)} - ${escapeHtml(i.status)}</li>`).join("")}</ul>`
            : "<p class='hint'>No invites yet.</p>"}
        </div>
      </div>
      <button id="refreshWorkspaceBtn" class="btn btn-small">Refresh workspace data</button>
    </section>
  `;
}

function renderGuestImportModal() {
  const modal = byId("guestImportModal");
  if (!modal) return;
  modal.classList.toggle("hidden", !uiState.guestImportPromptVisible);
}

function renderWorkspaceCryptoModal() {
  const modal = byId("workspaceCryptoModal");
  if (!modal) return;

  const auth = authStore.getState();
  const crypto = cryptoStore.getState();
  const shouldShow = uiState.workspaceCryptoPromptVisible && auth.mode === "authenticated" && isWorkspaceClientEncryptionEnabled();
  modal.classList.toggle("hidden", !shouldShow);
  if (!shouldShow) return;

  const title = byId("workspaceCryptoModalTitle");
  const body = byId("workspaceCryptoModalBody");
  const setupForm = byId("workspaceCryptoSetupForm");
  const unlockForm = byId("workspaceCryptoUnlockForm");
  const error = byId("workspaceCryptoErrorText");

  if (error) error.textContent = crypto.lastError || "";
  if (setupForm) setupForm.classList.add("hidden");
  if (unlockForm) unlockForm.classList.add("hidden");

  if (crypto.state === "setup_required") {
    if (title) title.textContent = "Set Workspace Passphrase";
    if (body) body.textContent = "Create a workspace passphrase. This passphrase is required each sign-in to unlock encrypted workspace data.";
    if (setupForm) setupForm.classList.remove("hidden");
    return;
  }

  if (title) title.textContent = "Unlock Workspace";
  if (body) body.textContent = "Enter your workspace passphrase to decrypt and use workspace data.";
  if (unlockForm) unlockForm.classList.remove("hidden");
}

function renderConnectionGateModal() {
  const modal = byId("connectionGateModal");
  if (!modal) return;

  if (readSettings().skipStartupWarmup) {
    modal.classList.add("hidden");
    return;
  }

  const conn = connectionStore.getState();
  modal.classList.toggle("hidden", !uiState.connectionGateVisible);

  const status = byId("connectionGateStatus");
  const message = byId("connectionGateMessage");
  const actionBtn = byId("gateRetryWarmupBtn");

  if (status) {
    const label = conn.state === "ready"
      ? "Ready"
      : conn.state === "warming"
        ? "Checking API readiness..."
        : conn.state === "pending"
          ? "Pending"
      : conn.state === "degraded"
        ? "Failed"
        : "Pending";
    status.textContent = label;
  }

  if (message) {
    message.textContent = conn.warmupText || conn.message || "Manual API check required before using the app.";
  }

  if (actionBtn) {
    actionBtn.disabled = Boolean(conn.warming);
    actionBtn.textContent = conn.warming ? "Checking..." : "Check API Status";
  }
}

function renderDay1Form(defaults = defaultDay1FormValues()) {
  const form = byId("day1Form");
  if (!form) return;

  form.innerHTML = BASELINE_FIELDS.map((field) => {
    const value = defaults[field.key] ?? "";
    if (field.type === "binary-radio") {
      const leftChecked = String(value) === "1" ? "checked" : "";
      const rightChecked = String(value) === "0" ? "checked" : "";
      return `
        <div class="field">
          <label id="${field.key}-label">${escapeHtml(field.label)}</label>
          <div class="pill-group" role="radiogroup" aria-labelledby="${field.key}-label">
            <input type="radio" id="${field.key}-left" name="${field.key}" value="1" ${leftChecked} />
            <label for="${field.key}-left" class="pill">${escapeHtml(field.options[0].label)}</label>
            <input type="radio" id="${field.key}-right" name="${field.key}" value="0" ${rightChecked} />
            <label for="${field.key}-right" class="pill">${escapeHtml(field.options[1].label)}</label>
          </div>
        </div>
      `;
    }

    const minAttr = hasValue(field.min) ? `min="${escapeHtml(field.min)}"` : "";
    const maxAttr = hasValue(field.max) ? `max="${escapeHtml(field.max)}"` : "";
    const readOnlyAttr = field.readonly ? "readonly" : "";
    return `
      <div class="field">
        <label for="${field.key}">${escapeHtml(field.label)}</label>
        <input id="${field.key}" name="${field.key}" type="number" step="${escapeHtml(field.step || "any")}" ${minAttr} ${maxAttr} ${readOnlyAttr} value="${escapeHtml(value)}" />
      </div>
    `;
  }).join("");
}

function renderDay2Form(prefill = {}) {
  const form = byId("day2Form");
  if (!form) return;

  form.innerHTML = DAY2_FIELDS.map((field) => {
    const value = Number(prefill[field.key] ?? 0);
    const leftChecked = value === 1 ? "checked" : "";
    const rightChecked = value === 0 ? "checked" : "";

    return `
      <div class="field">
        <label id="${field.key}-label">${escapeHtml(field.label)}</label>
        <div class="pill-group" role="radiogroup" aria-labelledby="${field.key}-label">
          <input type="radio" id="${field.key}-yes" name="${field.key}" value="1" ${leftChecked} />
          <label for="${field.key}-yes" class="pill">Received</label>
          <input type="radio" id="${field.key}-no" name="${field.key}" value="0" ${rightChecked} />
          <label for="${field.key}-no" class="pill">Not Received</label>
        </div>
      </div>
    `;
  }).join("");
}

function renderShell() {
  const root = byId("app");
  if (!root) return;

  root.innerHTML = `
    <div class="desktop-shell">
      <aside class="sidebar card">
        <div class="sidebar-brand">
          <img src="./banner-logo.png" alt="Sepsis Flow logo" class="sidebar-logo" />
        </div>

        <nav class="nav-list">
          <button id="nav-new-patient" class="nav-btn" data-page="new-patient">New Patient</button>
          <button id="nav-patients" class="nav-btn active" data-page="patients">Patients</button>
          <button id="nav-assess" class="nav-btn" data-page="assess">Assess</button>
          <button id="nav-settings" class="nav-btn" data-page="settings">Settings</button>
          <button id="nav-profile" class="nav-btn" data-page="profile">Profile</button>
        </nav>

        <section class="sidebar-status-box">
          <div class="status-head">
            <span id="sidebarStatusDot" class="status-dot status-warming"></span>
            <strong>System status</strong>
          </div>
          <p id="sidebarStatusText" class="status-text">Initializing application...</p>
          <section id="warmupCard" class="warmup-card">
            <div class="section-header">
              <h4>API Status Check</h4>
              <span id="warmupChip" class="chip">Pending</span>
            </div>
            <p id="warmupText" class="hint">API checks are manual. Click "Check API Status" to wake services and continue.</p>
            <div class="actions-row">
              <button id="retryWarmupBtn" class="btn btn-small" type="button">Check API Status</button>
            </div>
          </section>
          <span id="sidebarModeBadge" class="chip">Guest (local only)</span>
        </section>
      </aside>

      <main class="content-area">
        <section id="page-patients" class="page-section">
          <section class="card">
            <div class="section-header">
              <h2>Patients worklist</h2>
              <button id="goNewPatientBtn" class="btn btn-small">Add new patient</button>
            </div>
            <input id="patientSearchInput" placeholder="Search alias or external ID" />
          </section>

          <section class="patients-grid">
            <section class="card">
              <div class="section-header">
                <h2>Worklist</h2>
                <span id="patientsModeBadge" class="chip">Guest (local only)</span>
              </div>
              <p id="patientsError" class="error-text"></p>
              <div id="patientsList" class="list-wrap"></div>
            </section>

            <section class="card">
              <h2>Patient details</h2>
              <div id="patientDetailsPanel"></div>
            </section>
          </section>

          <section class="card">
            <h2>Patient timeline</h2>
            <div id="timelineList"></div>
          </section>
        </section>

        <section id="page-new-patient" class="page-section hidden">
          <section class="card">
            <h2>New patient</h2>
            <form id="createPatientForm" class="mini-form create-patient-form">
              <div class="grid-two">
                <input name="alias" placeholder="Alias (optional)" />
                <input name="externalId" placeholder="External ID (optional)" />
              </div>
              <div class="grid-two">
                <select name="country">
                  <option value="">Country (optional)</option>
                  ${STRATA_COUNTRIES.map((country) => `<option value="${escapeHtml(country)}">${escapeHtml(country)}</option>`).join("")}
                </select>
                <select name="inpatientStatus">
                  <option value="">Inpatient status (optional)</option>
                  <option value="Inpatient">Inpatient</option>
                  <option value="Outpatient">Outpatient</option>
                </select>
              </div>
              <div class="grid-three">
                <input name="ageMonths" type="number" min="0" step="1" placeholder="Age (months)" />
                <select name="sex" required>
                  <option value="" selected>Select sex (required)</option>
                  <option value="1">Male</option>
                  <option value="0">Female</option>
                </select>
                <input name="weightValue" type="number" min="0" step="0.01" placeholder="Weight value" />
              </div>
              <div class="grid-two">
                <select name="weightUnit">
                  <option value="">Weight unit</option>
                  <option value="kg">kg</option>
                  <option value="lbs">lbs</option>
                </select>
                <button class="btn btn-primary" type="submit">Create patient</button>
              </div>
            </form>
          </section>
        </section>

        <section id="page-assess" class="page-section hidden">
          <section class="card warmup-gateable">
            <h2>Assessment context</h2>
            <div class="grid-two">
              <div class="field">
                <label for="assessPatientSelect">Patient</label>
                <select id="assessPatientSelect"></select>
              </div>
              <div class="actions-row no-top-margin">
                <button id="exportAssessPatientBtn" class="btn" type="button">Export Patient</button>
              </div>
            </div>
            <p id="assessmentContextText" class="muted"></p>
          </section>

          <section class="card warmup-gateable">
            <h2>Day 1 baseline inputs</h2>
            <details>
              <summary>Optional prevalence adjustment</summary>
              <div class="grid-two">
                <div class="field">
                  <label for="priorCountry">Country</label>
                  <select id="priorCountry">
                    <option value="">Not set</option>
                    ${STRATA_COUNTRIES.map((country) => `<option value="${escapeHtml(country)}">${escapeHtml(country)}</option>`).join("")}
                  </select>
                </div>
                <div class="field">
                  <label for="priorInpatientStatus">Inpatient status</label>
                  <select id="priorInpatientStatus">
                    <option value="">Not set</option>
                    <option value="Inpatient">Inpatient</option>
                    <option value="Outpatient">Outpatient</option>
                  </select>
                </div>
              </div>
            </details>
            <form id="day1Form" class="grid-form"></form>
            <div class="actions-row">
              <button id="runDay1Btn" class="btn btn-primary" type="button">Run Day 1</button>
            </div>
          </section>

          <section class="card warmup-gateable">
            <h2>Day 1 results</h2>
            <div id="day1Results"></div>
          </section>

          <section class="card warmup-gateable">
            <h2>Day 2 carry-forward editor</h2>
            <p class="muted">Indicators are prefilled from Day 1 and can be overridden before Day 2 submission.</p>
            <form id="day2Form" class="grid-form compact"></form>
            <div class="actions-row">
              <button id="runDay2Btn" class="btn btn-primary" type="button">Run Day 2</button>
            </div>
          </section>

          <section class="card warmup-gateable">
            <h2>Day 2 results</h2>
            <div id="day2Results"></div>
          </section>

          <section class="card warmup-gateable">
            <h2>48-hour summary</h2>
            <div id="summary48h"></div>
          </section>
        </section>

        <section id="page-settings" class="page-section hidden">
          <section class="card">
            <h2>Authentication</h2>
            <p id="authErrorText" class="error-text"></p>
            <div id="authPanel"></div>
          </section>

          <section class="card">
            <h2>Runtime environment</h2>
            <p id="runtimeEnvironmentText" class="muted"></p>
            <p class="hint">Environment selection is deployment-controlled and not user-editable.</p>
          </section>

          <section class="card">
            <h2>Privacy defaults</h2>
            <label class="checkbox-row"><input id="privacyShowExternalIdByDefault" type="checkbox" /> Show external IDs by default</label>
            <div class="actions-row">
              <button id="savePrivacyBtn" class="btn">Save privacy settings</button>
            </div>
          </section>

          <section class="card">
            <h2>Guest data controls</h2>
            <p class="muted">Guest local data is device-scoped and separate from authenticated workspace data.</p>
            <div class="actions-row">
              <button id="clearGuestDataBtn" class="btn btn-danger">Clear all guest local data</button>
            </div>
            <div id="importResultPanel" class="muted small"></div>
          </section>

          <section class="card">
            <h2>Privacy and data sharing</h2>
            <p class="muted">Workspace data remains private by default. Support data sharing is opt-in.</p>
            <label class="checkbox-row"><input id="supportExportRedactExternalIds" type="checkbox" /> Redact external IDs in support package</label>
            <div class="field">
              <label for="supportExportScopeSelect">Support package scope</label>
              <select id="supportExportScopeSelect">
                <option value="selected">Selected patient only</option>
                <option value="all">All patients in current workspace/mode</option>
              </select>
            </div>
            <div class="actions-row">
              <button id="exportSupportPackageBtn" class="btn">Export support package</button>
              <button id="exportAllCsvBtn" class="btn">Export all CSV</button>
              <button id="exportJsonBtn" class="btn">Export JSON</button>
            </div>
          </section>

          <section class="card">
            <h2>Links and contact</h2>
            <div class="settings-links">
              <a href="https://github.com/ffr0517/sepsis-flow-platform" target="_blank" rel="noopener noreferrer">Platform repository</a>
              <a href="https://github.com/ffr0517/sepsis-flow-build" target="_blank" rel="noopener noreferrer">Model build repository</a>
              <a href="mailto:lcmrhodes98@gmail.com?subject=Sepsis%20Flow%20Feedback">Questions or comments by email</a>
            </div>
            <form id="feedbackEmailForm" class="mini-form feedback-form">
              <input name="subject" type="text" value="Sepsis Flow feedback" placeholder="Email subject" />
              <textarea name="message" rows="4" placeholder="Comments or questions"></textarea>
              <button class="btn" type="submit">Open email draft</button>
            </form>
          </section>
        </section>

        <section id="page-profile" class="page-section hidden">
          <div id="profileMainPanel"></div>
        </section>
      </main>
    </div>

    <div id="guestImportModal" class="modal hidden">
      <div class="modal-card">
        <h3>Import guest data?</h3>
        <p>Guest local patients/assessments were detected. Import them into this workspace?</p>
        <div class="actions-row">
          <button id="importGuestDataBtn" class="btn btn-primary">Import guest data</button>
          <button id="skipGuestImportBtn" class="btn">Skip for now</button>
        </div>
      </div>
    </div>

    <div id="connectionGateModal" class="modal connection-gate-modal">
      <div class="modal-card connection-gate-card">
        <h3>Manual API Status Check</h3>
        <p class="muted">Check backend readiness before using the app.</p>
        <p id="connectionGateStatus" class="connection-gate-status">Connection check pending.</p>
        <p id="connectionGateMessage" class="connection-gate-message">Manual API check required before using the app.</p>
        <div class="actions-row">
          <button id="gateRetryWarmupBtn" class="btn btn-primary">Check API Status</button>
        </div>
      </div>
    </div>

    <div id="workspaceCryptoModal" class="modal hidden workspace-crypto-modal">
      <div class="modal-card workspace-crypto-card">
        <h3 id="workspaceCryptoModalTitle">Unlock Workspace</h3>
        <p id="workspaceCryptoModalBody" class="muted"></p>
        <p id="workspaceCryptoErrorText" class="error-text"></p>

        <form id="workspaceCryptoSetupForm" class="mini-form hidden">
          <input name="passphrase" type="password" minlength="8" required placeholder="Create workspace passphrase" />
          <input name="passphraseConfirm" type="password" minlength="8" required placeholder="Confirm passphrase" />
          <button class="btn btn-primary" type="submit">Set passphrase and unlock</button>
        </form>

        <form id="workspaceCryptoUnlockForm" class="mini-form hidden">
          <input name="passphrase" type="password" required placeholder="Workspace passphrase" />
          <button class="btn btn-primary" type="submit">Unlock workspace</button>
        </form>
      </div>
    </div>
  `;

  renderDay1Form(defaultDay1FormValues());
  renderDay2Form({});
  renderNav();
  renderWarmupStatusCard();
  renderConnectionGateModal();
  renderWorkspaceCryptoModal();
}

async function maybePromptGuestImport() {
  const auth = authStore.getState();
  if (auth.mode !== "authenticated") {
    uiState.guestImportPromptVisible = false;
    renderGuestImportModal();
    return;
  }

  if (requiresWorkspaceUnlock() && !isWorkspaceUnlocked()) {
    uiState.guestImportPromptVisible = false;
    renderGuestImportModal();
    return;
  }

  const snapshot = await dataAccess.getGuestDataSnapshot();
  if (shouldPromptGuestImport(auth.user?.id, snapshot)) {
    uiState.guestImportPromptVisible = true;
    renderGuestImportModal();
  }
}

async function loadWorkspaceMeta() {
  if (authStore.getState().mode !== "authenticated") {
    workspaceStore.patch({ members: [], invites: [] });
    uiState.profile = null;
    renderProfileState();
    return;
  }

  try {
    const [members, invites, profile] = await Promise.all([
      dataAccess.listWorkspaceMembers(),
      dataAccess.listWorkspaceInvites(),
      dataAccess.getProfile()
    ]);
    workspaceStore.patch({ members, invites });
    uiState.profile = profile;
  } catch (error) {
    workspaceStore.patch({ error: error?.message || "Failed to load workspace metadata." });
  }

  renderProfileState();
}

async function ensureWorkspaceCryptoPromptState() {
  if (!isWorkspaceClientEncryptionEnabled()) {
    uiState.workspaceCryptoPromptVisible = false;
    renderWorkspaceCryptoModal();
    return;
  }

  const auth = authStore.getState();
  if (auth.mode !== "authenticated") {
    workspaceCryptoService.lockWorkspace();
    uiState.workspaceCryptoPromptVisible = false;
    uiState.workspaceCryptoPromptMode = "unlock";
    renderWorkspaceCryptoModal();
    return;
  }

  const info = await workspaceCryptoService.refreshLockState();
  patientStore.patch({
    patients: [],
    filteredPatients: [],
    selectedPatientId: null,
    selectedPatient: null,
    selectedPatientAssessments: [],
    error: info.requiresSetup
      ? "Set workspace passphrase to initialize encrypted storage."
      : "Workspace is locked. Unlock with passphrase to view workspace data."
  });
  uiState.workspaceCryptoPromptVisible = true;
  uiState.workspaceCryptoPromptMode = info.requiresSetup ? "setup" : "unlock";
  renderWorkspaceCryptoModal();
}

async function completeWorkspaceUnlockFlow() {
  uiState.workspaceCryptoPromptVisible = false;
  uiState.workspaceCryptoPromptMode = "unlock";
  renderWorkspaceCryptoModal();

  await refreshPatients();
  await loadWorkspaceMeta();
  await maybePromptGuestImport();
}

async function runManualConnectionCheck({ closeGateOnReady = false } = {}) {
  setStatus("Loading: checking API endpoints");
  renderConnectionGateModal();

  const conn = await connectionManager.checkReady();
  setStatus(conn.message || "Connection checked.");
  if (closeGateOnReady && conn.state === "ready") {
    uiState.connectionGateVisible = false;
    renderConnectionGateModal();
    if (authStore.getState().mode === "authenticated") {
      await maybePromptGuestImport();
    }
  } else {
    renderConnectionGateModal();
  }

  renderWarmupStatusCard();
  renderAssessState();
}

function hydrateFromAssessmentRecord(record) {
  if (!record) return;

  const patientId = record.patientId;
  uiState.assessPatientId = patientId;

  const day1Envelope = { data: { day1_result: record.day1Outputs || [] }, trace: record.modelMetadata?.day1Trace || null };
  const day2Envelope = record.day2Outputs
    ? { data: { day2_result: record.day2Outputs || [] }, trace: record.modelMetadata?.day2Trace || null }
    : null;

  assessmentStore.patch({
    draft: { ...record },
    baselineInputs: record.baselineInputs || null,
    day1Response: day1Envelope,
    day2Response: day2Envelope,
    day2Prefill: record.day2CarryForwardEdited || {},
    editedDay2CarryForward: record.day2CarryForwardEdited || {},
    priorAdjustments: record.strata || null,
    unsavedBannerVisible: false,
    lastSavedAssessmentId: record.id
  });

  renderDay1Form({ ...defaultDay1FormValues(), ...(record.baselineInputs || {}) });
  renderDay2Form(record.day2CarryForwardEdited || {});
  if (record.strata?.country) byId("priorCountry").value = record.strata.country;
  if (record.strata?.inpatient_status) byId("priorInpatientStatus").value = record.strata.inpatient_status;
  renderAssessState();
}

async function handleClick(event) {
  const target = event.target;
  if (!(target instanceof HTMLElement)) return;

  const navButton = target.closest(".nav-btn");
  if (navButton) {
    const nextPage = navButton.dataset.page || "patients";
    if (nextPage === "assess" && uiState.currentPage !== "assess") {
      selectAssessPatient(null);
    }
    uiState.currentPage = nextPage;
    renderNav();
    if (uiState.currentPage === "profile") renderProfileState();
    return;
  }

  if (target.id === "retryWarmupBtn" || target.id === "gateRetryWarmupBtn") {
    await runManualConnectionCheck({
      closeGateOnReady: target.id === "gateRetryWarmupBtn"
    });
    return;
  }

  if (target.id === "runDay1Btn") {
    try {
      await runDay1();
    } catch {
      // handled in runDay1
    }
    return;
  }

  if (target.id === "runDay2Btn") {
    try {
      await runDay2();
    } catch {
      // handled in runDay2
    }
    return;
  }

  if (target.id === "exportAssessPatientBtn") {
    ensureWorkspaceUnlocked();
    ensureConnectionReady();
    const patient = getAssessPatient();
    if (!patient) {
      setStatus("Select a patient first.");
      return;
    }
    const assessments = await dataAccess.listAssessmentsByPatient(patient.id);
    const csv = buildCsv([patient], assessments);
    downloadText(`sepsis-flow-${patient.alias}-timeline.csv`, csv, "text/csv;charset=utf-8;");
    setStatus("Patient CSV exported.");
    return;
  }

  if (target.id === "exportAllCsvBtn") {
    ensureWorkspaceUnlocked();
    ensureConnectionReady();
    const [patients, assessments] = await Promise.all([
      dataAccess.listPatients(""),
      dataAccess.getAllAssessments()
    ]);
    const csv = buildCsv(patients, assessments);
    downloadText("sepsis-flow-all-patients.csv", csv, "text/csv;charset=utf-8;");
    setStatus("All-patient CSV exported.");
    return;
  }

  if (target.id === "exportJsonBtn") {
    ensureWorkspaceUnlocked();
    ensureConnectionReady();
    const [patients, assessments] = await Promise.all([
      dataAccess.listPatients(""),
      dataAccess.getAllAssessments()
    ]);
    const payload = {
      exportedAt: new Date().toISOString(),
      mode: authStore.getState().mode,
      workspace: workspaceStore.getState().workspace || null,
      patients,
      assessments
    };
    downloadText("sepsis-flow-export.json", JSON.stringify(payload, null, 2), "application/json;charset=utf-8;");
    setStatus("JSON export downloaded.");
    return;
  }

  if (target.id === "exportSupportPackageBtn") {
    ensureWorkspaceUnlocked();
    ensureConnectionReady();
    const scope = uiState.supportExportScope || "selected";
    const redact = Boolean(uiState.supportExportRedactExternalIds);

    let patients = [];
    let assessments = [];

    if (scope === "selected") {
      const selected = patientStore.getState().selectedPatient;
      if (!selected) {
        setStatus("Open a patient first or switch support package scope to all patients.");
        return;
      }
      patients = [selected];
      assessments = await dataAccess.listAssessmentsByPatient(selected.id);
    } else {
      [patients, assessments] = await Promise.all([
        dataAccess.listPatients(""),
        dataAccess.getAllAssessments()
      ]);
    }

    const pkg = buildSupportPackage({
      patients,
      assessments,
      scope,
      redactExternalIds: redact
    });

    downloadText(
      `sepsis-flow-support-package-${scope}.json`,
      JSON.stringify(pkg, null, 2),
      "application/json;charset=utf-8;"
    );
    setStatus("Support package exported.");
    return;
  }

  if (target.id === "goNewPatientBtn") {
    uiState.currentPage = "new-patient";
    renderNav();
    return;
  }

  if (target.matches("[data-action='select-patient']")) {
    ensureWorkspaceUnlocked();
    const patientId = target.dataset.patientId;
    const selected = patientStore.getState().patients.find((row) => row.id === patientId) || null;
    patientStore.patch({ selectedPatientId: patientId, selectedPatient: selected });
    await refreshTimeline(patientId);
    renderPatientsState();
    return;
  }

  if (target.matches("[data-action='open-in-assess']")) {
    ensureWorkspaceUnlocked();
    const patientId = target.dataset.patientId;
    uiState.currentPage = "assess";
    renderNav();
    selectAssessPatient(patientId);
    return;
  }

  if (target.matches("[data-action='clear-selected-patient']")) {
    patientStore.patch({ selectedPatientId: null, selectedPatient: null, selectedPatientAssessments: [] });
    renderPatientsState();
    return;
  }

  if (target.matches("[data-action='delete-patient']")) {
    ensureWorkspaceUnlocked();
    const patientId = target.dataset.patientId;
    if (!patientId) return;
    if (!window.confirm("Delete this patient and all timeline assessments?")) return;

    try {
      await dataAccess.deletePatient(patientId);
      if (patientStore.getState().selectedPatientId === patientId) {
        patientStore.patch({ selectedPatientId: null, selectedPatient: null, selectedPatientAssessments: [] });
      }
      if (uiState.assessPatientId === patientId) {
        selectAssessPatient(null);
      }
      await refreshPatients();
      setStatus("Patient deleted.");
    } catch (error) {
      setStatus(error?.message || "Delete failed.");
    }
    return;
  }

  if (target.matches("[data-action='open-assessment']")) {
    ensureWorkspaceUnlocked();
    const id = target.dataset.assessmentId;
    const row = patientStore.getState().selectedPatientAssessments.find((item) => item.id === id);
    uiState.selectedAssessmentId = id;
    hydrateFromAssessmentRecord(row);
    uiState.currentPage = "assess";
    renderNav();
    renderPatientsState();
    return;
  }

  if (target.id === "savePrivacyBtn") {
    settingsStore.patch({
      privacyShowExternalIdByDefault: Boolean(byId("privacyShowExternalIdByDefault")?.checked)
    });
    persistSettingsToLocal();
    renderPatientsState();
    setStatus("Privacy defaults updated.");
    return;
  }

  if (target.id === "clearGuestDataBtn") {
    if (!window.confirm("Clear all guest local patients and assessments?")) return;
    await dataAccess.clearAllGuestData();
    await refreshPatients();
    setStatus("Guest local data cleared.");
    return;
  }

  if (target.id === "signOutBtn") {
    workspaceCryptoService.lockWorkspace();
    uiState.workspaceCryptoPromptVisible = false;
    uiState.workspaceCryptoPromptMode = "unlock";
    renderWorkspaceCryptoModal();
    await authService.signOut();
    uiState.assessPatientId = null;
    renderSettingsState();
    renderProfileState();
    await refreshPatients();
    selectAssessPatient(null);
    setStatus("Signed out. Guest mode active.");
    return;
  }

  if (target.id === "guestModeBtn") {
    workspaceCryptoService.lockWorkspace();
    uiState.workspaceCryptoPromptVisible = false;
    uiState.workspaceCryptoPromptMode = "unlock";
    renderWorkspaceCryptoModal();
    setGuestMode();
    uiState.assessPatientId = null;
    renderSettingsState();
    renderProfileState();
    await refreshPatients();
    selectAssessPatient(null);
    setStatus("Guest mode active.");
    return;
  }

  if (target.id === "importGuestDataBtn") {
    const auth = authStore.getState();
    if (auth.mode !== "authenticated") return;

    uiState.loading.import = true;
    try {
      const snapshot = await dataAccess.getGuestDataSnapshot();
      const result = await importGuestDataIntoWorkspace({
        guestSnapshot: snapshot,
        createPatient: (payload) => dataAccess.createPatient(payload),
        upsertAssessment: (record) => dataAccess.upsertAssessment(record)
      });
      uiState.guestImportResult = result;
      markGuestImportDecision(auth.user?.id);
      uiState.guestImportPromptVisible = false;
      renderGuestImportModal();
      await refreshPatients();
      renderSettingsState();
      setStatus(`Imported ${result.importedPatients} patients and ${result.importedAssessments} assessments.`);
    } catch (error) {
      setStatus(error?.message || "Guest import failed.");
    } finally {
      uiState.loading.import = false;
    }
    return;
  }

  if (target.id === "skipGuestImportBtn") {
    const auth = authStore.getState();
    markGuestImportDecision(auth.user?.id);
    uiState.guestImportPromptVisible = false;
    renderGuestImportModal();
    setStatus("Skipped guest data import.");
    return;
  }

  if (target.id === "refreshWorkspaceBtn") {
    ensureWorkspaceUnlocked();
    await loadWorkspaceMeta();
    setStatus("Workspace metadata refreshed.");
    return;
  }

  if (target.id === "resetEncryptedWorkspaceBtn") {
    const isOwner = workspaceStore.getState().membershipRole === "owner";
    if (!isOwner) {
      setStatus("Only workspace owners can reset encrypted workspace data.");
      return;
    }
    const confirmed = window.confirm("Reset encrypted workspace data? This removes all workspace patients and assessments.");
    if (!confirmed) return;
    const typed = window.prompt("Type RESET to confirm.");
    if (typed !== "RESET") return;

    try {
      await workspaceCryptoService.resetWorkspaceEncryptedData();
      uiState.workspaceCryptoPromptVisible = true;
      uiState.workspaceCryptoPromptMode = "setup";
      selectAssessPatient(null);
      await refreshPatients();
      renderWorkspaceCryptoModal();
      setStatus("Encrypted workspace data was reset.");
    } catch (error) {
      setStatus(error?.message || "Workspace reset failed.");
    }
    return;
  }
}

async function handleSubmit(event) {
  const form = event.target;
  if (!(form instanceof HTMLFormElement)) return;
  event.preventDefault();

  if (form.id === "createPatientForm") {
    ensureWorkspaceUnlocked();
    const formData = new FormData(form);
    const alias = String(formData.get("alias") || "");
    const externalId = String(formData.get("externalId") || "");

    const profile = readProfileFromCreatePatientForm(formData);
    if (profile.sex === null) {
      setStatus("Sex is required before creating a patient.");
      return;
    }

    try {
      await dataAccess.createPatient({ alias, externalId, ...profile });
      form.reset();
      await refreshPatients();
      uiState.currentPage = "patients";
      renderNav();
      setStatus("Patient saved.");
    } catch (error) {
      setStatus(error?.message || "Could not save patient.");
    }
    return;
  }

  if (form.id === "editPatientForm") {
    ensureWorkspaceUnlocked();
    const formData = new FormData(form);
    const patientId = String(formData.get("patientId") || "").trim();
    const alias = String(formData.get("alias") || "").trim();
    const externalId = String(formData.get("externalId") || "");

    if (!patientId) {
      setStatus("No patient selected to update.");
      return;
    }

    if (!alias) {
      setStatus("Alias is required when editing a patient.");
      return;
    }

    const profile = readProfileFromCreatePatientForm(formData);

    try {
      await dataAccess.updatePatient(patientId, { alias, externalId, ...profile });
      await refreshPatients();
      const selected = patientStore.getState().patients.find((row) => row.id === patientId) || null;
      patientStore.patch({ selectedPatientId: patientId, selectedPatient: selected });
      await refreshTimeline(patientId);
      setStatus("Patient updated.");
    } catch (error) {
      setStatus(error?.message || "Could not update patient.");
    }
    return;
  }

  if (form.id === "signInForm") {
    const formData = new FormData(form);
    const email = String(formData.get("email") || "").trim();
    const password = String(formData.get("password") || "");

    try {
      await authService.signIn(email, password);
      await loadWorkspaceMeta();
      await ensureWorkspaceCryptoPromptState();
      if (isWorkspaceUnlocked() || !requiresWorkspaceUnlock()) {
        await refreshPatients();
        await maybePromptGuestImport();
      }
      renderSettingsState();
      renderProfileState();
      setStatus("Signed in successfully.");
    } catch (error) {
      setAuthError(error?.message || "Sign in failed.");
      renderSettingsState();
      setStatus(error?.message || "Sign in failed.");
    }
    return;
  }

  if (form.id === "signUpForm") {
    const formData = new FormData(form);
    const email = String(formData.get("email") || "").trim();
    const password = String(formData.get("password") || "");
    const workspaceName = String(formData.get("workspaceName") || "").trim();

    try {
      if (workspaceName) {
        setPendingWorkspaceNameForEmail(email, workspaceName);
      }
      await authService.signUp(email, password);
      form.reset();
      setStatus("Sign-up submitted. Check email to verify your account.");
    } catch (error) {
      setStatus(error?.message || "Sign-up failed.");
    }
    return;
  }

  if (form.id === "resetPasswordForm") {
    const formData = new FormData(form);
    const email = String(formData.get("email") || "").trim();

    try {
      await authService.resetPassword(email);
      setStatus("Password reset email sent.");
    } catch (error) {
      setStatus(error?.message || "Reset request failed.");
    }
    return;
  }

  if (form.id === "workspaceCryptoSetupForm") {
    const formData = new FormData(form);
    const passphrase = String(formData.get("passphrase") || "");
    const confirm = String(formData.get("passphraseConfirm") || "");
    if (passphrase !== confirm) {
      setStatus("Passphrase confirmation does not match.");
      return;
    }

    try {
      await workspaceCryptoService.initializeWorkspacePassphrase(passphrase);
      form.reset();
      setStatus("Workspace encryption initialized.");
      await completeWorkspaceUnlockFlow();
    } catch (error) {
      setStatus(error?.message || "Could not initialize workspace encryption.");
      renderWorkspaceCryptoModal();
    }
    return;
  }

  if (form.id === "workspaceCryptoUnlockForm") {
    const formData = new FormData(form);
    const passphrase = String(formData.get("passphrase") || "");

    try {
      await workspaceCryptoService.unlockWorkspace(passphrase);
      form.reset();
      setStatus("Workspace unlocked.");
      await completeWorkspaceUnlockFlow();
    } catch (error) {
      setStatus(error?.message || "Workspace unlock failed.");
      renderWorkspaceCryptoModal();
    }
    return;
  }

  if (form.id === "feedbackEmailForm") {
    const formData = new FormData(form);
    const subject = String(formData.get("subject") || "Sepsis Flow feedback").trim() || "Sepsis Flow feedback";
    const message = String(formData.get("message") || "").trim();
    const mailto = `mailto:lcmrhodes98@gmail.com?subject=${encodeURIComponent(subject)}&body=${encodeURIComponent(message)}`;
    window.location.href = mailto;
    setStatus("Email draft opened.");
    return;
  }

  if (form.id === "profileForm") {
    ensureWorkspaceUnlocked();
    const formData = new FormData(form);
    const displayName = String(formData.get("displayName") || "").trim();

    try {
      uiState.profile = await dataAccess.upsertProfile({ displayName });
      renderProfileState();
      setStatus("Profile updated.");
    } catch (error) {
      setStatus(error?.message || "Could not update profile.");
    }
    return;
  }

  if (form.id === "workspaceRenameForm") {
    ensureWorkspaceUnlocked();
    const formData = new FormData(form);
    const workspaceName = String(formData.get("workspaceName") || "").trim();

    try {
      const updated = await dataAccess.updateWorkspaceName(workspaceName);
      workspaceStore.patch({ workspace: updated });
      renderProfileState();
      setStatus("Workspace name updated.");
    } catch (error) {
      setStatus(error?.message || "Could not rename workspace.");
    }
    return;
  }

  if (form.id === "profileInviteForm") {
    ensureWorkspaceUnlocked();
    const formData = new FormData(form);
    const email = String(formData.get("email") || "").trim();

    try {
      await dataAccess.sendInvite(email);
      form.reset();
      await loadWorkspaceMeta();
      setStatus(`Invite sent to ${email}.`);
    } catch (error) {
      setStatus(error?.message || "Invite failed.");
    }
  }
}

function handleInput(event) {
  const target = event.target;
  if (!(target instanceof HTMLElement)) return;

  if (target.id === "patientSearchInput") {
    const search = String(target.value || "");
    patientStore.patch({ search });
    void refreshPatients();
    return;
  }

  if (target.id === "assessPatientSelect") {
    selectAssessPatient(String(target.value || ""));
    return;
  }

  if (target.id === "supportExportRedactExternalIds") {
    uiState.supportExportRedactExternalIds = Boolean(target.checked);
    return;
  }

  if (target.id === "supportExportScopeSelect") {
    uiState.supportExportScope = String(target.value || "selected");
    return;
  }

  const sourceKey = target.getAttribute("name") || target.getAttribute("id") || "";
  if (!WFAZ_AUTOFILL_SOURCE_KEYS.has(sourceKey)) return;

  const wfazState = assessmentStore.getState().wfazCalc;
  if (wfazState.debounceId) clearTimeout(wfazState.debounceId);

  const debounceId = setTimeout(async () => {
    const nextState = assessmentStore.getState().wfazCalc;
    assessmentStore.patch({ wfazCalc: { ...nextState, debounceId: null } });
    try {
      await ensureWfazReadyForDay1();
    } catch {
      // Inline auto-fill failure is handled at submit time.
    }
  }, 300);

  assessmentStore.patch({ wfazCalc: { ...wfazState, debounceId } });
}

function bindEvents() {
  const root = byId("app");
  root.addEventListener("click", (event) => {
    void handleClick(event).catch((error) => {
      setStatus(error?.message || "Action failed.");
    });
  });
  root.addEventListener("submit", (event) => {
    void handleSubmit(event).catch((error) => {
      setStatus(error?.message || "Submit failed.");
    });
  });
  root.addEventListener("input", handleInput);
}

function subscribeStores() {
  authStore.subscribe(() => {
    renderSettingsState();
    renderGlobalStatus();
    renderProfileState();
    renderWorkspaceCryptoModal();
  });

  settingsStore.subscribe(() => {
    renderSettingsState();
    renderWarmupStatusCard();
    renderConnectionGateModal();
  });

  patientStore.subscribe(() => {
    renderPatientsState();
    renderAssessState();
  });

  assessmentStore.subscribe(() => {
    renderAssessState();
  });

  connectionStore.subscribe((state) => {
    setStatus(state.message || "Status updated.");
    renderAssessState();
    renderWarmupStatusCard();
    renderConnectionGateModal();
  });

  workspaceStore.subscribe(() => {
    renderProfileState();
    renderPatientsState();
    renderGlobalStatus();
  });

  cryptoStore.subscribe(() => {
    renderWorkspaceCryptoModal();
    renderPatientsState();
    renderAssessState();
  });
}

async function init() {
  const persisted = loadPersistedSettings();
  const runtimeSkipWarmup = Boolean(runtimeConfig.appConfig.skipStartupWarmup);
  settingsStore.patch({
    ...defaultSettings(),
    ...(persisted || {}),
    skipStartupWarmup: runtimeSkipWarmup
  });

  renderShell();
  bindEvents();
  subscribeStores();

  renderSettingsState();
  renderPatientsState();
  renderAssessState();
  renderProfileState();
  renderGlobalStatus();

  try {
    await authService.init();
  } catch (error) {
    setStatus(error?.message || "Authentication initialization failed.");
  }

  await loadWorkspaceMeta();
  await ensureWorkspaceCryptoPromptState();
  if (!requiresWorkspaceUnlock() || isWorkspaceUnlocked()) {
    await refreshPatients();
    await maybePromptGuestImport();
  }

  if (runtimeSkipWarmup) {
    uiState.connectionGateVisible = false;
    await connectionManager.checkReady();
    setStatus("Local mode active. Startup warmup skipped.");
  } else {
    uiState.connectionGateVisible = true;
    setStatus("APIs are idle. Click 'Check API Status' to wake services and continue.");
  }

  renderWarmupStatusCard();
  renderConnectionGateModal();
  renderWorkspaceCryptoModal();
}

void init();
