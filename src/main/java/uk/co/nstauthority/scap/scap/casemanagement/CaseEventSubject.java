package uk.co.nstauthority.scap.scap.casemanagement;

public enum CaseEventSubject {
  QA_COMMENT(CaseEventAction.QA, "QA comment added"),
  FURTHER_INFO_REQUESTED(CaseEventAction.INFO_REQUESTED, "Addional Information Requested"),
  SCAP_SUBMITTED(CaseEventAction.SUBMIT, "Submitted SCAP"),
  CONSULTATION_REQUESTED(CaseEventAction.CONSULTATION_REQUESTED, "Consultation Requested");


  private String displayName;

  private String caseEventAction;

  CaseEventSubject(String buttonName, String eventDisplayName) {
    this.caseEventAction = buttonName;
    displayName = eventDisplayName;
  }

  public String getDisplayName() {
    return displayName;
  }

  public String getCaseEventAction() {
    return caseEventAction;
  }
}
