package uk.co.nstauthority.scap.scap.casemanagement;

import uk.co.fivium.digitalenummaterialisationlibrary.enummaterialisation.MaterialisableEnum;

public enum CaseEventGroups implements MaterialisableEnum {
  CONSULTATIONS("Consultations"),
  UPDATE_SCAP("Update SCAP"),
  DECISIONS("Decisions"),
  FURTHER_INFO("Further info"),
  QA("QA");

  private final String displayName;

  CaseEventGroups(String displayName) {
    this.displayName = displayName;
  }

  @Override
  public String getDisplayName() {
    return displayName;
  }
}
