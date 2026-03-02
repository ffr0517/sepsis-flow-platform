import test from "node:test";
import assert from "node:assert/strict";
import { createWorkspaceCryptoService } from "../src/services/workspaceCryptoService.js";
import { cryptoStore } from "../src/state/cryptoStore.js";

function makeMockDataAccess() {
  const settings = new Map();
  return {
    settings,
    async getAppSetting(key) {
      return settings.get(key) ?? null;
    },
    async upsertAppSetting(key, value) {
      settings.set(key, value);
    },
    async deleteAppSetting(key) {
      settings.delete(key);
    },
    async resetWorkspaceEncryptedData() {}
  };
}

function resetCryptoStore() {
  cryptoStore.setState({
    state: "locked",
    workspaceId: null,
    key: null,
    meta: null,
    lastError: null
  });
}

test("refreshLockState marks setup_required when no crypto metadata exists", async () => {
  resetCryptoStore();
  const dataAccess = makeMockDataAccess();
  const service = createWorkspaceCryptoService({
    dataAccess,
    getAuthState: () => ({ mode: "authenticated", user: { id: "u1" } }),
    getWorkspaceState: () => ({ workspace: { id: "w1" }, membershipRole: "owner" }),
    featureEnabled: () => true
  });

  const result = await service.refreshLockState();
  assert.equal(result.requiresSetup, true);
  assert.equal(cryptoStore.getState().state, "setup_required");
});

test("initialize and unlock workspace passphrase lifecycle", async () => {
  resetCryptoStore();
  const dataAccess = makeMockDataAccess();
  const service = createWorkspaceCryptoService({
    dataAccess,
    getAuthState: () => ({ mode: "authenticated", user: { id: "u1" } }),
    getWorkspaceState: () => ({ workspace: { id: "w1" }, membershipRole: "owner" }),
    featureEnabled: () => true
  });

  await service.initializeWorkspacePassphrase("passphrase-123");
  assert.equal(cryptoStore.getState().state, "unlocked");

  service.lockWorkspace();
  assert.equal(cryptoStore.getState().state, "locked");

  await service.unlockWorkspace("passphrase-123");
  assert.equal(cryptoStore.getState().state, "unlocked");
});
