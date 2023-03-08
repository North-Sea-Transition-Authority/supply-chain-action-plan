package uk.co.nstauthority.scap.scap.casemanagement;

public enum CaseEventGroups {
  CONSULTATIONS("Consultations"),
  UPDATE_SCAP("Update SCAP"),
  DECISIONS("Decisions"),
  FURTHER_INFO("Further info"),
  QA("QA");

  private final String displayName;

  CaseEventGroups(String displayName) {
    this.displayName = displayName;
  }

  public String getDisplayName() {
    return displayName;
  }
}
