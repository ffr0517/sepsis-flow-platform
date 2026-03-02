import { createStore } from "./createStore.js";

/**
 * @typedef {'locked'|'unlocking'|'unlocked'|'setup_required'|'error'} WorkspaceCryptoState
 */

export const cryptoStore = createStore({
  state: "locked",
  workspaceId: null,
  key: null,
  meta: null,
  lastError: null
});

export function setCryptoLocked(workspaceId = null, meta = null) {
  cryptoStore.patch({
    state: "locked",
    workspaceId,
    key: null,
    meta: meta || null,
    lastError: null
  });
}

export function setCryptoSetupRequired(workspaceId = null) {
  cryptoStore.patch({
    state: "setup_required",
    workspaceId,
    key: null,
    meta: null,
    lastError: null
  });
}

export function setCryptoUnlocking(workspaceId = null, meta = null) {
  cryptoStore.patch({
    state: "unlocking",
    workspaceId,
    meta: meta || null,
    lastError: null
  });
}

export function setCryptoUnlocked({ workspaceId, key, meta }) {
  cryptoStore.patch({
    state: "unlocked",
    workspaceId,
    key,
    meta: meta || null,
    lastError: null
  });
}

export function setCryptoError(message) {
  cryptoStore.patch({
    state: "error",
    key: null,
    lastError: String(message || "Workspace encryption error.")
  });
}
