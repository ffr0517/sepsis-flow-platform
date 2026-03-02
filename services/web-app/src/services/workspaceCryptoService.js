import {
  createKeyCheckEnvelope,
  deriveWorkspaceKey,
  generateSaltB64,
  kdfDefaults,
  verifyKeyCheckEnvelope
} from "./cryptoService.js";
import {
  cryptoStore,
  setCryptoError,
  setCryptoLocked,
  setCryptoSetupRequired,
  setCryptoUnlocked,
  setCryptoUnlocking
} from "../state/cryptoStore.js";

const CRYPTO_META_KEY = "workspace_crypto_meta";

function safeTrim(value) {
  return String(value || "").trim();
}

export function createWorkspaceCryptoService({
  dataAccess,
  getAuthState,
  getWorkspaceState,
  featureEnabled
}) {
  function isEnabled() {
    return Boolean(featureEnabled?.());
  }

  function currentWorkspaceId() {
    return getWorkspaceState?.()?.workspace?.id || null;
  }

  function requireAuthWorkspace() {
    const auth = getAuthState?.();
    const workspace = getWorkspaceState?.();
    if (auth?.mode !== "authenticated") throw new Error("Sign in is required.");
    if (!workspace?.workspace?.id) throw new Error("Workspace context not found.");
    return {
      userId: auth.user?.id || null,
      workspaceId: workspace.workspace.id,
      role: workspace.membershipRole || null
    };
  }

  async function fetchCryptoMeta() {
    if (!isEnabled()) return null;
    return dataAccess.getAppSetting(CRYPTO_META_KEY);
  }

  async function refreshLockState() {
    if (!isEnabled()) {
      setCryptoUnlocked({
        workspaceId: currentWorkspaceId(),
        key: null,
        meta: null
      });
      return { requiresSetup: false, meta: null };
    }

    const auth = getAuthState?.();
    if (auth?.mode !== "authenticated") {
      setCryptoLocked();
      return { requiresSetup: false, meta: null };
    }

    const workspaceId = currentWorkspaceId();
    const meta = await fetchCryptoMeta();
    if (!meta) {
      setCryptoSetupRequired(workspaceId);
      return { requiresSetup: true, meta: null };
    }

    setCryptoLocked(workspaceId, meta);
    return { requiresSetup: false, meta };
  }

  async function initializeWorkspacePassphrase(passphrase) {
    const cleanPassphrase = safeTrim(passphrase);
    if (cleanPassphrase.length < 8) {
      throw new Error("Passphrase must be at least 8 characters.");
    }

    const { userId, workspaceId, role } = requireAuthWorkspace();
    if (role !== "owner") throw new Error("Only workspace owners can initialize encryption.");

    setCryptoUnlocking(workspaceId, null);
    const existing = await fetchCryptoMeta();
    if (existing) {
      throw new Error("Workspace encryption is already initialized.");
    }

    const kdf = {
      ...kdfDefaults(),
      salt_b64: generateSaltB64()
    };
    const key = await deriveWorkspaceKey(cleanPassphrase, kdf.salt_b64, kdf.iterations);
    const keyCheck = await createKeyCheckEnvelope(key, workspaceId);

    const meta = {
      enc_v: 1,
      kdf,
      key_check: keyCheck,
      initialized_at: new Date().toISOString(),
      initialized_by: userId
    };

    await dataAccess.upsertAppSetting(CRYPTO_META_KEY, meta);
    setCryptoUnlocked({
      workspaceId,
      key,
      meta
    });
    return meta;
  }

  async function unlockWorkspace(passphrase) {
    const cleanPassphrase = safeTrim(passphrase);
    if (!cleanPassphrase) throw new Error("Enter your workspace passphrase.");

    const { workspaceId } = requireAuthWorkspace();
    const meta = await fetchCryptoMeta();
    if (!meta) {
      setCryptoSetupRequired(workspaceId);
      throw new Error("Workspace encryption has not been initialized yet.");
    }

    setCryptoUnlocking(workspaceId, meta);
    const key = await deriveWorkspaceKey(cleanPassphrase, meta?.kdf?.salt_b64, meta?.kdf?.iterations);
    const valid = await verifyKeyCheckEnvelope(key, meta.key_check, workspaceId);
    if (!valid) {
      setCryptoError("Incorrect workspace passphrase.");
      throw new Error("Incorrect workspace passphrase.");
    }

    setCryptoUnlocked({
      workspaceId,
      key,
      meta
    });
    return { key, meta };
  }

  function lockWorkspace() {
    const workspaceId = currentWorkspaceId();
    const meta = cryptoStore.getState().meta || null;
    setCryptoLocked(workspaceId, meta);
  }

  function getCryptoContext() {
    const state = cryptoStore.getState();
    const workspaceId = currentWorkspaceId();
    return {
      enabled: isEnabled(),
      workspaceId,
      key: state.state === "unlocked" && state.workspaceId === workspaceId ? state.key : null
    };
  }

  async function resetWorkspaceEncryptedData() {
    const { role, workspaceId } = requireAuthWorkspace();
    if (role !== "owner") throw new Error("Only workspace owners can reset workspace encrypted data.");
    await dataAccess.resetWorkspaceEncryptedData();
    await dataAccess.deleteAppSetting(CRYPTO_META_KEY);
    setCryptoSetupRequired(workspaceId);
  }

  return {
    isEnabled,
    refreshLockState,
    initializeWorkspacePassphrase,
    unlockWorkspace,
    lockWorkspace,
    getCryptoContext,
    resetWorkspaceEncryptedData
  };
}

export { CRYPTO_META_KEY };
