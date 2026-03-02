import { uid } from "../utils/format.js";
import { decryptJson, encryptJson } from "../services/cryptoService.js";

const ENCRYPTED_PATIENT_ALIAS = "Encrypted patient";

function mapError(error, fallback = "Workspace operation failed.") {
  return new Error(error?.message || fallback);
}

function asNullable(value) {
  if (value === null || value === undefined) return null;
  const out = String(value).trim();
  return out ? out : null;
}

function asNullableNumber(value) {
  if (value === null || value === undefined || String(value).trim() === "") return null;
  const n = Number(value);
  return Number.isFinite(n) ? n : null;
}

function normalizeSex(value) {
  if (value === null || value === undefined || String(value).trim() === "") return null;
  const n = Number(value);
  if (!Number.isFinite(n)) return null;
  return n > 0.5 ? 1 : 0;
}

function normalizeWeightUnit(value) {
  const text = asNullable(value);
  if (!text) return null;
  const normalized = text.toLowerCase();
  if (normalized === "kg") return "kg";
  if (normalized === "lbs" || normalized === "lb") return "lbs";
  return null;
}

function normalizeInpatientStatus(value) {
  const text = asNullable(value);
  if (!text) return null;
  if (/^inpatient$/i.test(text)) return "Inpatient";
  if (/^outpatient$/i.test(text)) return "Outpatient";
  return null;
}

function mapPatientRow(row) {
  if (!row) return null;
  return {
    id: row.id,
    alias: row.alias,
    externalId: row.external_id,
    createdAt: row.created_at,
    updatedAt: row.updated_at,
    lastAssessmentAt: row.last_assessment_at,
    country: row.country,
    inpatientStatus: row.inpatient_status,
    ageMonths: row.age_months,
    sex: row.sex,
    weightValue: row.weight_value,
    weightUnit: row.weight_unit,
    securePayload: row.secure_payload || null
  };
}

function mapAssessmentRow(row) {
  if (!row) return null;
  return {
    id: row.id,
    patientId: row.patient_id,
    status: row.status,
    environment: row.environment,
    orchestratorBaseUrl: row.orchestrator_base_url,
    baselineInputs: row.baseline_inputs,
    day1Outputs: row.day1_outputs,
    day2CarryForwardEdited: row.day2_carry_forward_edited,
    day2Outputs: row.day2_outputs,
    strata: row.strata,
    summary48h: row.summary_48h,
    modelMetadata: row.model_metadata,
    createdBy: row.created_by,
    createdAt: row.created_at,
    updatedAt: row.updated_at,
    securePayload: row.secure_payload || null
  };
}

function normalizePatientPayload(input = {}) {
  return {
    alias: asNullable(input.alias),
    external_id: asNullable(input.externalId),
    country: asNullable(input.country),
    inpatient_status: normalizeInpatientStatus(input.inpatientStatus),
    age_months: asNullableNumber(input.ageMonths),
    sex: normalizeSex(input.sex),
    weight_value: asNullableNumber(input.weightValue),
    weight_unit: normalizeWeightUnit(input.weightUnit)
  };
}

function hasOwn(obj, key) {
  return Object.prototype.hasOwnProperty.call(obj, key);
}

function filterPatientsLocal(rows, search = "") {
  const term = String(search || "").trim().toLowerCase();
  if (!term) return rows;
  return rows.filter((row) => {
    const alias = String(row.alias || "").toLowerCase();
    const external = String(row.externalId || "").toLowerCase();
    return alias.includes(term) || external.includes(term);
  });
}

function scrubPatientColumns(payload = {}) {
  return {
    alias: ENCRYPTED_PATIENT_ALIAS,
    external_id: null,
    country: null,
    inpatient_status: null,
    age_months: null,
    sex: null,
    weight_value: null,
    weight_unit: null
  };
}

function scrubAssessmentColumns() {
  return {
    environment: null,
    orchestrator_base_url: null,
    baseline_inputs: {},
    day1_outputs: [],
    day2_carry_forward_edited: {},
    day2_outputs: null,
    strata: null,
    summary_48h: {},
    model_metadata: {}
  };
}

export function createWorkspaceRepository(
  getSupabase,
  getAuthContext,
  getWorkspaceContext,
  getCryptoContext = () => ({ enabled: false, key: null, workspaceId: null }),
  getWorkspaceRole = () => null
) {
  async function getContext() {
    const supabase = getSupabase();
    const auth = getAuthContext();
    const workspace = getWorkspaceContext();

    if (!supabase) throw new Error("Supabase is not configured.");
    if (!auth?.user?.id) throw new Error("You must be signed in.");
    if (!workspace?.id) throw new Error("No active workspace found.");

    return {
      supabase,
      userId: auth.user.id,
      workspaceId: workspace.id
    };
  }

  function getCryptoState(workspaceId) {
    const cryptoContext = getCryptoContext?.() || {};
    const enabled = Boolean(cryptoContext.enabled);
    if (!enabled) return { enabled: false, key: null };
    if (cryptoContext.workspaceId && cryptoContext.workspaceId !== workspaceId) {
      throw new Error("Workspace encryption context mismatch.");
    }
    if (!cryptoContext.key) {
      throw new Error("Workspace is locked. Unlock with passphrase first.");
    }
    return { enabled: true, key: cryptoContext.key };
  }

  async function decryptPatientRowIfNeeded(row, workspaceId, key) {
    if (!row) return null;
    const mapped = mapPatientRow(row);
    if (!mapped.securePayload) return mapped;
    const decrypted = await decryptJson(mapped.securePayload, key, `patients:${workspaceId}:${mapped.id}`);
    return {
      ...mapped,
      alias: decrypted.alias || ENCRYPTED_PATIENT_ALIAS,
      externalId: decrypted.externalId || null,
      country: decrypted.country || null,
      inpatientStatus: decrypted.inpatientStatus || null,
      ageMonths: decrypted.ageMonths ?? null,
      sex: decrypted.sex ?? null,
      weightValue: decrypted.weightValue ?? null,
      weightUnit: decrypted.weightUnit || null
    };
  }

  async function decryptAssessmentRowIfNeeded(row, workspaceId, key) {
    if (!row) return null;
    const mapped = mapAssessmentRow(row);
    if (!mapped.securePayload) return mapped;
    const decrypted = await decryptJson(mapped.securePayload, key, `assessments:${workspaceId}:${mapped.id}`);
    return {
      ...mapped,
      patientId: decrypted.patientId,
      status: decrypted.status,
      environment: decrypted.environment,
      orchestratorBaseUrl: decrypted.orchestratorBaseUrl,
      baselineInputs: decrypted.baselineInputs,
      day1Outputs: decrypted.day1Outputs,
      day2CarryForwardEdited: decrypted.day2CarryForwardEdited,
      day2Outputs: decrypted.day2Outputs,
      strata: decrypted.strata,
      summary48h: decrypted.summary48h,
      modelMetadata: decrypted.modelMetadata,
      createdBy: decrypted.createdBy || mapped.createdBy
    };
  }

  return {
    async listPatients(search = "") {
      const { supabase, workspaceId } = await getContext();
      const crypto = getCryptoState(workspaceId);

      let query = supabase
        .from("patients")
        .select("*")
        .eq("workspace_id", workspaceId)
        .order("last_assessment_at", { ascending: false, nullsFirst: false })
        .order("created_at", { ascending: false });

      const term = String(search || "").trim();
      if (term && !crypto.enabled) {
        query = query.or(`alias.ilike.%${term}%,external_id.ilike.%${term}%`);
      }

      const { data, error } = await query;
      if (error) throw mapError(error, "Failed to load patients.");
      const rows = data || [];
      if (!crypto.enabled) return rows.map(mapPatientRow);

      const decrypted = [];
      for (const row of rows) {
        decrypted.push(await decryptPatientRowIfNeeded(row, workspaceId, crypto.key));
      }
      return filterPatientsLocal(decrypted, search);
    },

    async createPatient({ alias, externalId = "", ...profileInput }) {
      const { supabase, userId, workspaceId } = await getContext();
      const crypto = getCryptoState(workspaceId);
      const now = new Date().toISOString();
      const normalized = normalizePatientPayload({ alias, externalId, ...profileInput });
      const finalAlias = normalized.alias || `Patient-${String(Date.now()).slice(-6)}`;
      const id = uid();

      const payload = {
        id,
        workspace_id: workspaceId,
        created_by: userId,
        created_at: now,
        updated_at: now,
        last_assessment_at: null
      };

      if (crypto.enabled) {
        const securePayload = await encryptJson({
          alias: finalAlias,
          externalId: normalized.external_id,
          country: normalized.country,
          inpatientStatus: normalized.inpatient_status,
          ageMonths: normalized.age_months,
          sex: normalized.sex,
          weightValue: normalized.weight_value,
          weightUnit: normalized.weight_unit
        }, crypto.key, `patients:${workspaceId}:${id}`);
        Object.assign(payload, scrubPatientColumns(), { secure_payload: securePayload });
      } else {
        Object.assign(payload, {
          alias: finalAlias,
          external_id: normalized.external_id,
          country: normalized.country,
          inpatient_status: normalized.inpatient_status,
          age_months: normalized.age_months,
          sex: normalized.sex,
          weight_value: normalized.weight_value,
          weight_unit: normalized.weight_unit
        });
      }

      const { data, error } = await supabase.from("patients").insert(payload).select("*").single();
      if (error) throw mapError(error, "Failed to create patient.");
      if (!crypto.enabled) return mapPatientRow(data);
      return decryptPatientRowIfNeeded(data, workspaceId, crypto.key);
    },

    async updatePatient(id, updates) {
      const { supabase, workspaceId } = await getContext();
      const crypto = getCryptoState(workspaceId);
      const normalized = normalizePatientPayload(updates);
      const { data: existing, error: existingError } = await supabase
        .from("patients")
        .select("*")
        .eq("workspace_id", workspaceId)
        .eq("id", id)
        .single();
      if (existingError) throw mapError(existingError, "Failed to load patient before update.");

      const payload = { updated_at: new Date().toISOString() };
      if (crypto.enabled) {
        const current = await decryptPatientRowIfNeeded(existing, workspaceId, crypto.key);
        const merged = {
          alias: hasOwn(updates, "alias") ? (normalized.alias || current.alias) : current.alias,
          externalId: hasOwn(updates, "externalId") ? normalized.external_id : current.externalId,
          country: hasOwn(updates, "country") ? normalized.country : current.country,
          inpatientStatus: hasOwn(updates, "inpatientStatus") ? normalized.inpatient_status : current.inpatientStatus,
          ageMonths: hasOwn(updates, "ageMonths") ? normalized.age_months : current.ageMonths,
          sex: hasOwn(updates, "sex") ? normalized.sex : current.sex,
          weightValue: hasOwn(updates, "weightValue") ? normalized.weight_value : current.weightValue,
          weightUnit: hasOwn(updates, "weightUnit") ? normalized.weight_unit : current.weightUnit
        };

        const securePayload = await encryptJson(merged, crypto.key, `patients:${workspaceId}:${id}`);
        Object.assign(payload, scrubPatientColumns(), { secure_payload: securePayload });
      } else {
        if (hasOwn(updates, "alias")) payload.alias = normalized.alias || existing.alias;
        if (hasOwn(updates, "externalId")) payload.external_id = normalized.external_id;
        if (hasOwn(updates, "country")) payload.country = normalized.country;
        if (hasOwn(updates, "inpatientStatus")) payload.inpatient_status = normalized.inpatient_status;
        if (hasOwn(updates, "ageMonths")) payload.age_months = normalized.age_months;
        if (hasOwn(updates, "sex")) payload.sex = normalized.sex;
        if (hasOwn(updates, "weightValue")) payload.weight_value = normalized.weight_value;
        if (hasOwn(updates, "weightUnit")) payload.weight_unit = normalized.weight_unit;
      }

      const { data, error } = await supabase
        .from("patients")
        .update(payload)
        .eq("workspace_id", workspaceId)
        .eq("id", id)
        .select("*")
        .single();

      if (error) throw mapError(error, "Failed to update patient.");
      if (!crypto.enabled) return mapPatientRow(data);
      return decryptPatientRowIfNeeded(data, workspaceId, crypto.key);
    },

    async deletePatient(id) {
      const { supabase, workspaceId } = await getContext();
      const { error } = await supabase
        .from("patients")
        .delete()
        .eq("workspace_id", workspaceId)
        .eq("id", id);
      if (error) throw mapError(error, "Failed to delete patient.");
    },

    async listAssessmentsByPatient(patientId) {
      const { supabase, workspaceId } = await getContext();
      const crypto = getCryptoState(workspaceId);
      const { data, error } = await supabase
        .from("assessments")
        .select("*")
        .eq("workspace_id", workspaceId)
        .eq("patient_id", patientId)
        .order("created_at", { ascending: false });
      if (error) throw mapError(error, "Failed to load assessments.");
      const rows = data || [];
      if (!crypto.enabled) return rows.map(mapAssessmentRow);
      const out = [];
      for (const row of rows) {
        out.push(await decryptAssessmentRowIfNeeded(row, workspaceId, crypto.key));
      }
      return out;
    },

    async upsertAssessment(record) {
      const { supabase, workspaceId, userId } = await getContext();
      const crypto = getCryptoState(workspaceId);
      const now = new Date().toISOString();
      const id = record.id || uid();
      const payload = {
        id,
        workspace_id: workspaceId,
        patient_id: record.patientId,
        status: record.status,
        created_by: record.createdBy || userId,
        created_at: record.createdAt || now,
        updated_at: now
      };

      if (crypto.enabled) {
        const securePayload = await encryptJson({
          patientId: record.patientId,
          status: record.status,
          environment: record.environment,
          orchestratorBaseUrl: record.orchestratorBaseUrl,
          baselineInputs: record.baselineInputs,
          day1Outputs: record.day1Outputs,
          day2CarryForwardEdited: record.day2CarryForwardEdited,
          day2Outputs: record.day2Outputs,
          strata: record.strata || null,
          summary48h: record.summary48h,
          modelMetadata: record.modelMetadata,
          createdBy: record.createdBy || userId
        }, crypto.key, `assessments:${workspaceId}:${id}`);

        Object.assign(payload, scrubAssessmentColumns(), { secure_payload: securePayload });
      } else {
        Object.assign(payload, {
          environment: record.environment,
          orchestrator_base_url: record.orchestratorBaseUrl,
          baseline_inputs: record.baselineInputs,
          day1_outputs: record.day1Outputs,
          day2_carry_forward_edited: record.day2CarryForwardEdited,
          day2_outputs: record.day2Outputs,
          strata: record.strata || null,
          summary_48h: record.summary48h,
          model_metadata: record.modelMetadata
        });
      }

      const { data, error } = await supabase
        .from("assessments")
        .upsert(payload)
        .select("*")
        .single();
      if (error) throw mapError(error, "Failed to save assessment.");

      await supabase
        .from("patients")
        .update({
          last_assessment_at: now,
          updated_at: now
        })
        .eq("workspace_id", workspaceId)
        .eq("id", record.patientId);

      if (!crypto.enabled) return mapAssessmentRow(data);
      return decryptAssessmentRowIfNeeded(data, workspaceId, crypto.key);
    },

    async getAllAssessments() {
      const { supabase, workspaceId } = await getContext();
      const crypto = getCryptoState(workspaceId);
      const { data, error } = await supabase
        .from("assessments")
        .select("*")
        .eq("workspace_id", workspaceId)
        .order("created_at", { ascending: false });
      if (error) throw mapError(error, "Failed to load assessments.");
      const rows = data || [];
      if (!crypto.enabled) return rows.map(mapAssessmentRow);
      const out = [];
      for (const row of rows) {
        out.push(await decryptAssessmentRowIfNeeded(row, workspaceId, crypto.key));
      }
      return out;
    },

    async getProfile() {
      const { supabase, userId } = await getContext();
      const { data, error } = await supabase
        .from("profiles")
        .select("user_id,display_name,created_at,updated_at")
        .eq("user_id", userId)
        .maybeSingle();
      if (error) throw mapError(error, "Failed to load user profile.");
      return data || { user_id: userId, display_name: null };
    },

    async upsertProfile({ displayName }) {
      const { supabase, userId } = await getContext();
      const { data, error } = await supabase
        .from("profiles")
        .upsert({
          user_id: userId,
          display_name: asNullable(displayName)
        })
        .select("user_id,display_name,created_at,updated_at")
        .single();
      if (error) throw mapError(error, "Failed to save profile.");
      return data;
    },

    async updateWorkspaceName(name) {
      const { supabase, workspaceId } = await getContext();
      const cleanName = asNullable(name);
      if (!cleanName) throw new Error("Workspace name is required.");

      const { data, error } = await supabase
        .from("workspaces")
        .update({ name: cleanName, updated_at: new Date().toISOString() })
        .eq("id", workspaceId)
        .select("id,name,created_at,updated_at,created_by")
        .single();

      if (error) throw mapError(error, "Failed to update workspace name.");
      return data;
    },

    async listWorkspaceMembers() {
      const { supabase, workspaceId } = await getContext();
      const { data, error } = await supabase
        .from("workspace_members")
        .select("workspace_id,user_id,role,status,created_at")
        .eq("workspace_id", workspaceId)
        .eq("status", "active")
        .order("created_at", { ascending: true });
      if (error) throw mapError(error, "Failed to load workspace members.");
      return data || [];
    },

    async listWorkspaceInvites() {
      const { supabase, workspaceId } = await getContext();
      const { data, error } = await supabase
        .from("workspace_invites")
        .select("id,email,status,expires_at,created_at,invited_by")
        .eq("workspace_id", workspaceId)
        .in("status", ["pending", "accepted"])
        .order("created_at", { ascending: false });
      if (error) throw mapError(error, "Failed to load invites.");
      return data || [];
    },

    async sendInvite(email) {
      const { supabase, workspaceId } = await getContext();
      const { data, error } = await supabase.functions.invoke("send-workspace-invite", {
        body: { workspaceId, email }
      });
      if (error) throw mapError(error, "Failed to send invite.");
      return data;
    },

    async acceptInvite(inviteId) {
      const { supabase } = await getContext();
      const { data, error } = await supabase.functions.invoke("accept-workspace-invite", {
        body: { inviteId }
      });
      if (error) throw mapError(error, "Failed to accept invite.");
      return data;
    },

    async upsertAppSetting(key, value) {
      const { supabase, workspaceId } = await getContext();
      const { error } = await supabase
        .from("app_settings")
        .upsert({
          workspace_id: workspaceId,
          key,
          value,
          updated_at: new Date().toISOString()
        });
      if (error) throw mapError(error, "Failed to save workspace setting.");
    },

    async getAppSetting(key) {
      const { supabase, workspaceId } = await getContext();
      const { data, error } = await supabase
        .from("app_settings")
        .select("value")
        .eq("workspace_id", workspaceId)
        .eq("key", key)
        .maybeSingle();
      if (error) throw mapError(error, "Failed to load workspace setting.");
      return data?.value ?? null;
    },

    async deleteAppSetting(key) {
      const { supabase, workspaceId } = await getContext();
      const { error } = await supabase
        .from("app_settings")
        .delete()
        .eq("workspace_id", workspaceId)
        .eq("key", key);
      if (error) throw mapError(error, "Failed to delete workspace setting.");
    },

    async resetWorkspaceEncryptedData() {
      const { supabase, workspaceId } = await getContext();
      const role = getWorkspaceRole?.();
      if (role !== "owner") {
        throw new Error("Only workspace owners can reset encrypted workspace data.");
      }

      const { error: deleteAssessmentsError } = await supabase
        .from("assessments")
        .delete()
        .eq("workspace_id", workspaceId);
      if (deleteAssessmentsError) throw mapError(deleteAssessmentsError, "Failed to clear assessments.");

      const { error: deletePatientsError } = await supabase
        .from("patients")
        .delete()
        .eq("workspace_id", workspaceId);
      if (deletePatientsError) throw mapError(deletePatientsError, "Failed to clear patients.");
    }
  };
}
