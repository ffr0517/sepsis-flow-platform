/**
 * Routes persistence calls to guest IndexedDB or workspace Supabase repository.
 */
export function createDataAccessFacade({ getMode, guestRepository, workspaceRepository }) {
  function activeRepo() {
    return getMode() === "authenticated" ? workspaceRepository : guestRepository;
  }

  return {
    listPatients(search = "") {
      return activeRepo().listPatients(search);
    },
    createPatient(payload) {
      return activeRepo().createPatient(payload);
    },
    updatePatient(id, payload) {
      return activeRepo().updatePatient(id, payload);
    },
    deletePatient(id) {
      return activeRepo().deletePatient(id);
    },
    listAssessmentsByPatient(patientId) {
      return activeRepo().listAssessmentsByPatient(patientId);
    },
    upsertAssessment(record) {
      return activeRepo().upsertAssessment(record);
    },
    getAllAssessments() {
      return activeRepo().getAllAssessments();
    },
    clearAllGuestData() {
      return guestRepository.clearAllGuestData();
    },
    getGuestDataSnapshot() {
      return guestRepository.getGuestDataSnapshot();
    },
    listWorkspaceMembers() {
      if (getMode() !== "authenticated") return Promise.resolve([]);
      return workspaceRepository.listWorkspaceMembers();
    },
    listWorkspaceInvites() {
      if (getMode() !== "authenticated") return Promise.resolve([]);
      return workspaceRepository.listWorkspaceInvites();
    },
    sendInvite(email) {
      if (getMode() !== "authenticated") throw new Error("Sign in is required.");
      return workspaceRepository.sendInvite(email);
    },
    acceptInvite(inviteId) {
      if (getMode() !== "authenticated") throw new Error("Sign in is required.");
      return workspaceRepository.acceptInvite(inviteId);
    },
    getProfile() {
      if (getMode() !== "authenticated") return Promise.resolve(null);
      return workspaceRepository.getProfile();
    },
    upsertProfile(payload) {
      if (getMode() !== "authenticated") throw new Error("Sign in is required.");
      return workspaceRepository.upsertProfile(payload);
    },
    updateWorkspaceName(name) {
      if (getMode() !== "authenticated") throw new Error("Sign in is required.");
      return workspaceRepository.updateWorkspaceName(name);
    },
    upsertAppSetting(key, value) {
      if (getMode() !== "authenticated") return Promise.resolve();
      return workspaceRepository.upsertAppSetting(key, value);
    },
    getAppSetting(key) {
      if (getMode() !== "authenticated") return Promise.resolve(null);
      return workspaceRepository.getAppSetting(key);
    },
    deleteAppSetting(key) {
      if (getMode() !== "authenticated") return Promise.resolve();
      return workspaceRepository.deleteAppSetting(key);
    },
    resetWorkspaceEncryptedData() {
      if (getMode() !== "authenticated") throw new Error("Sign in is required.");
      return workspaceRepository.resetWorkspaceEncryptedData();
    }
  };
}
