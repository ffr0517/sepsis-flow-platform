const ORCHESTRATOR_API_BASE_URL = "https://sepsis-flow-orchestrator.onrender.com";

const BASELINE_FIELDS = [
  { key: "age.months", label: "Age (months)", type: "number", step: "1" },
  { key: "sex", label: "Sex (1=male, 0=female)", type: "number", step: "1", min: "0", max: "1" },
  { key: "adm.recent", label: "Recent Admission (1/0)", type: "number", step: "1", min: "0", max: "1" },
  { key: "wfaz", label: "WFA Z-Score", type: "number", step: "0.01" },
  { key: "cidysymp", label: "Illness Duration (days)", type: "number", step: "1" },
  { key: "not.alert", label: "Not Alert (1/0)", type: "number", step: "1", min: "0", max: "1" },
  { key: "hr.all", label: "Heart Rate", type: "number", step: "0.1" },
  { key: "rr.all", label: "Respiratory Rate", type: "number", step: "0.1" },
  { key: "envhtemp", label: "Temperature (C)", type: "number", step: "0.1" },
  { key: "crt.long", label: "CRT >2s (1/0)", type: "number", step: "1", min: "0", max: "1" },
  { key: "oxy.ra", label: "SpO2 (%)", type: "number", step: "0.1" }
];

const DAY2_FIELDS = [
  { key: "LEVEL1_TREATMENTS_D1_SAFE_0", label: "Day 1: Mechanical ventilation, inotropes, or renal replacement therapy" },
  { key: "LEVEL2_TREATMENTS_D1_SAFE_0", label: "Day 1: CPAP or IV fluid bolus" },
  { key: "LEVEL3_TREATMENTS_D1_SAFE_0", label: "Day 1: ICU admission with clinical reason" },
  { key: "LEVEL4_TREATMENTS_D1_SAFE_0", label: "Day 1: O2 via face or nasal cannula" },
  { key: "LEVEL5_TREATMENTS_D1_SAFE_0", label: "Day 1: Non-bolused IV fluids" }
];

const state = {
  baselineInputs: null,
  day2Prefill: null,
  day1Response: null,
  day2Response: null,
  startupReady: false,
  startupWarming: false,
  loading: {
    day1: false,
    day2: false
  }
};

const byId = (id) => document.getElementById(id);

function makeFieldHtml({ key, label, type = "number", step = "any", min, max }, value = "") {
  const minAttr = min !== undefined ? `min="${min}"` : "";
  const maxAttr = max !== undefined ? `max="${max}"` : "";
  return `
    <div class="field">
      <label for="${key}">${label}</label>
      <input id="${key}" name="${key}" type="${type}" step="${step}" ${minAttr} ${maxAttr} value="${value}" required />
    </div>
  `;
}

function renderDay1Form(defaults = {}) {
  byId("day1Form").innerHTML = BASELINE_FIELDS.map((f) => makeFieldHtml(f, defaults[f.key] ?? "")).join("");
}

function renderDay2Form(prefill = {}) {
  byId("day2Form").innerHTML = DAY2_FIELDS.map((f) => makeFieldHtml({ ...f, min: "0", max: "1", step: "1" }, prefill[f.key] ?? 0)).join("");
}

function showCard(id) {
  byId(id).classList.remove("hidden");
}

function setStatus(kind, message) {
  const indicator = byId("statusIndicator");
  const text = byId("statusText");
  indicator.className = `status-indicator status-${kind}`;
  text.textContent = message;
}

function setLoading(phase, isLoading) {
  state.loading[phase] = isLoading;
  const runDay1Btn = byId("runDay1Btn");
  const runDay2Btn = byId("runDay2Btn");

  if (phase === "day1") {
    runDay1Btn.disabled = isLoading;
    runDay1Btn.textContent = isLoading ? "Running Day 1..." : "Run Day 1";
  }
  if (phase === "day2") {
    runDay2Btn.disabled = isLoading;
    runDay2Btn.textContent = isLoading ? "Running Day 2..." : "Run Day 2";
  }

  if (state.loading.day1 || state.loading.day2) {
    setStatus("loading", "Loading: running prediction request. Please wait.");
  }
}

function setWarmupUi({ text, chipLabel, chipClass }) {
  const warmupText = byId("warmupText");
  const warmupChip = byId("warmupChip");
  warmupText.textContent = text;
  warmupChip.textContent = chipLabel;
  warmupChip.className = `chip ${chipClass}`.trim();
}

function setInteractionLocked(locked) {
  const gateableCards = document.querySelectorAll(".gateable");
  gateableCards.forEach((card) => {
    card.classList.toggle("locked", locked);
    card.querySelectorAll("input, button, select, textarea").forEach((el) => {
      el.disabled = locked;
    });
  });
}

function readNumberInput(id) {
  const raw = byId(id).value;
  const n = Number(raw);
  if (!Number.isFinite(n)) throw new Error(`Invalid numeric value for ${id}.`);
  return n;
}

function collectBaselineInputs() {
  const out = {};
  BASELINE_FIELDS.forEach((f) => {
    out[f.key] = readNumberInput(f.key);
  });
  return out;
}

function collectDay2Prefill() {
  const out = {};
  DAY2_FIELDS.forEach((f) => {
    const value = readNumberInput(f.key);
    out[f.key] = value > 0.5 ? 1 : 0;
  });
  return out;
}

function asPercent(value) {
  const num = Number(value);
  if (!Number.isFinite(num)) return "";
  return (num * 100).toFixed(2);
}

function formatPredictionRow(row) {
  return {
    treatment: row.level ?? "",
    avgPredictedProbabilityPct: asPercent(row.mean_predicted_probability),
    votersExceedingThreshold: row.votes_exceeding_threshold ?? "",
    votesAboveThresholdPct: asPercent(row.votes_above_threshold),
    overallTreatmentPrediction: row.predicted_treatment_by_majority_vote ? "Yes" : "No"
  };
}

function tableFromRows(rows) {
  if (!Array.isArray(rows) || rows.length === 0) return "<p class='hint'>No rows returned.</p>";
  const formatted = rows.map(formatPredictionRow);
  const header = `
    <th>Treatment</th>
    <th>Averaged Predicted Probability (%)</th>
    <th>Voters Exceeding Threshold</th>
    <th>Votes Above Threshold (%)</th>
    <th>Overall Treatment Prediction</th>
  `;
  const body = formatted
    .map((row) => {
      return `
        <tr>
          <td>${row.treatment}</td>
          <td>${row.avgPredictedProbabilityPct}</td>
          <td>${row.votersExceedingThreshold}</td>
          <td>${row.votesAboveThresholdPct}</td>
          <td>${row.overallTreatmentPrediction}</td>
        </tr>
      `;
    })
    .join("");

  return `
    <div class="table-wrap">
      <table><thead><tr>${header}</tr></thead><tbody>${body}</tbody></table>
    </div>
    ${summaryCardsFromRows(formatted)}
  `;
}

function summaryCardsFromRows(rows) {
  const cards = rows
    .map((row) => {
      return `
        <article class="summary-card">
          <h3>${row.treatment || "Treatment"}</h3>
          <p><strong>Averaged Predicted Probability:</strong> ${row.avgPredictedProbabilityPct}%</p>
          <p><strong>Voters Exceeding Threshold:</strong> ${row.votersExceedingThreshold}</p>
          <p><strong>Votes Above Threshold:</strong> ${row.votesAboveThresholdPct}%</p>
          <p><strong>Overall Treatment Prediction:</strong> ${row.overallTreatmentPrediction}</p>
        </article>
      `;
    })
    .join("");

  return `<div class="summary-cards">${cards}</div>`;
}

async function postJson(url, payload) {
  const resp = await fetch(url, {
    method: "POST",
    headers: { "Content-Type": "application/json", Accept: "application/json" },
    body: JSON.stringify(payload)
  });
  const body = await resp.json().catch(() => ({}));
  if (!resp.ok) {
    throw new Error(body?.error?.message || body?.error || `Request failed with HTTP ${resp.status}.`);
  }
  return body;
}

function renderDay1Results(envelope) {
  const rows = envelope?.data?.day1_result || [];
  byId("day1Results").innerHTML = tableFromRows(rows);
  showCard("day1ResultsCard");
}

function renderDay2Results(envelope) {
  const rows = envelope?.data?.day2_result || [];
  byId("day2Results").innerHTML = tableFromRows(rows);
  showCard("day2ResultsCard");
}

function escapeCsvCell(value) {
  const str = String(value ?? "");
  if (/[",\n]/.test(str)) return `"${str.replace(/"/g, '""')}"`;
  return str;
}

function buildCsvRows() {
  const header = [
    "Day",
    "Treatment",
    "Averaged Predicted Probability (%)",
    "Voters Exceeding Threshold",
    "Votes Above Threshold (%)",
    "Overall Treatment Prediction"
  ];
  const lines = [header.map(escapeCsvCell).join(",")];

  const day1Rows = (state.day1Response?.data?.day1_result || []).map((row) => ({ day: "Day 1", ...formatPredictionRow(row) }));
  const day2Rows = (state.day2Response?.data?.day2_result || []).map((row) => ({ day: "Day 2", ...formatPredictionRow(row) }));

  [...day1Rows, ...day2Rows].forEach((row) => {
    const values = [
      row.day,
      row.treatment,
      row.avgPredictedProbabilityPct,
      row.votersExceedingThreshold,
      row.votesAboveThresholdPct,
      row.overallTreatmentPrediction
    ];
    lines.push(values.map(escapeCsvCell).join(","));
  });

  return lines.join("\n");
}

function downloadCsv(filename, csvText) {
  const blob = new Blob([csvText], { type: "text/csv;charset=utf-8;" });
  const url = URL.createObjectURL(blob);
  const a = document.createElement("a");
  a.href = url;
  a.download = filename;
  document.body.appendChild(a);
  a.click();
  a.remove();
  URL.revokeObjectURL(url);
}

async function handleRunDay1() {
  try {
    if (!state.startupReady) throw new Error("Startup check is still running. Wait for APIs to be ready.");
    setLoading("day1", true);
    const baselineInputs = collectBaselineInputs();
    const envelope = await postJson(`${ORCHESTRATOR_API_BASE_URL}/flow/day1?format=long`, { data: baselineInputs });

    state.baselineInputs = envelope?.data?.baseline_inputs || baselineInputs;
    state.day2Prefill = envelope?.data?.day2_prefill || {};
    state.day1Response = envelope;
    state.day2Response = null;

    renderDay1Results(envelope);
    renderDay2Form(state.day2Prefill);
    showCard("day2EditCard");
    byId("day2ResultsCard").classList.add("hidden");
    byId("exportCard").classList.add("hidden");
    setStatus("success", "Success: Day 1 treatment predictions completed.");
  } catch (err) {
    setStatus("error", `Failed: ${err.message}`);
  } finally {
    setLoading("day1", false);
  }
}

async function handleRunDay2() {
  try {
    if (!state.startupReady) throw new Error("Startup check is still running. Wait for APIs to be ready.");
    setLoading("day2", true);
    if (!state.baselineInputs) throw new Error("Run Day 1 first to generate baseline and Day 2 prefill.");
    const day2Prefill = collectDay2Prefill();

    const envelope = await postJson(`${ORCHESTRATOR_API_BASE_URL}/flow/day2?format=long`, {
      baseline_inputs: state.baselineInputs,
      day2_prefill: day2Prefill
    });

    state.day2Prefill = day2Prefill;
    state.day2Response = envelope;

    renderDay2Results(envelope);
    showCard("exportCard");
    setStatus("success", "Success: Day 2 treatment predictions completed.");
  } catch (err) {
    setStatus("error", `Failed: ${err.message}`);
  } finally {
    setLoading("day2", false);
  }
}

function handleExport() {
  if (!state.startupReady) {
    setStatus("error", "Failed: startup check has not completed yet.");
    return;
  }
  if (!state.day1Response || !state.day2Response) {
    setStatus("error", "Failed: run both Day 1 and Day 2 predictions before exporting CSV.");
    return;
  }
  const csvText = buildCsvRows();
  downloadCsv("sepsis-flow-two-day-results.csv", csvText);
  setStatus("success", "Success: CSV export downloaded.");
}

async function runStartupWarmup() {
  if (state.startupWarming) return;
  state.startupWarming = true;
  state.startupReady = false;
  setInteractionLocked(true);
  byId("retryWarmupBtn").disabled = true;
  setStatus("loading", "Loading: waking up Day 1 and Day 2 APIs before enabling the form.");
  setWarmupUi({
    text: "Checking Day 1 and Day 2 APIs. This can take up to a few minutes on free-tier cold start.",
    chipLabel: "Warming Up",
    chipClass: "chip-warn"
  });

  try {
    await postJson(`${ORCHESTRATOR_API_BASE_URL}/warmup`, {});
    state.startupReady = true;
    setInteractionLocked(false);
    setStatus("neutral", "Ready to run Day 1 prediction.");
    setWarmupUi({
      text: "Startup check complete. Day 1 and Day 2 APIs are ready.",
      chipLabel: "Ready",
      chipClass: "chip-ok"
    });
  } catch (err) {
    state.startupReady = false;
    setInteractionLocked(true);
    setStatus("error", `Failed: ${err.message}`);
    setWarmupUi({
      text: `Startup check failed: ${err.message}`,
      chipLabel: "Failed",
      chipClass: "chip-error"
    });
  } finally {
    state.startupWarming = false;
    byId("retryWarmupBtn").disabled = false;
  }
}

function init() {
  renderDay1Form({
    "age.months": 24,
    sex: 0,
    "adm.recent": 0,
    wfaz: -1.1,
    cidysymp: 2,
    "not.alert": 0,
    "hr.all": 120,
    "rr.all": 28,
    envhtemp: 37.8,
    "crt.long": 0,
    "oxy.ra": 98
  });

  byId("runDay1Btn").addEventListener("click", handleRunDay1);
  byId("runDay2Btn").addEventListener("click", handleRunDay2);
  byId("exportBtn").addEventListener("click", handleExport);
  byId("retryWarmupBtn").addEventListener("click", runStartupWarmup);
  runStartupWarmup();
}

init();
