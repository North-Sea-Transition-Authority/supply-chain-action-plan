package uk.co.nstauthority.scap.scap.casemanagement;

import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventAction.APPROVED;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventAction.INFO_REQUESTED;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventAction.INFO_RESPONSE;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventAction.QA;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventAction.SUBMIT;

public enum CaseEventSubject {
  QA_COMMENT(QA,
      "QA checks completed",
      "Complete QA checks",
      "Qa-Panel"),
  FURTHER_INFO_REQUESTED(INFO_REQUESTED,
      "Addional Information Requested",
      "Request further information",
      "Info-Request-Panel"),

  FURTHER_INFO_RESPONSE(INFO_RESPONSE,
      "Additional Information Response",
      null,
      null),
  SCAP_SUBMITTED(SUBMIT,
      "Submitted SCAP",
      null,
      null),
  CONSULTATION_REQUESTED(CaseEventAction.CONSULTATION_REQUESTED,
      "Consultation Requested",
      "Request Consultation",
      "Consultation-Request-Panel"),
  SCAP_APPROVED(APPROVED,
      "SCAP Approved",
      null,
      null);


  private String displayName;

  private String caseEventAction;

  private String buttonText;

  private String actionPanelId;

  CaseEventSubject(String buttonName, String eventDisplayName, String buttonText, String actionPanelId) {
    this.caseEventAction = buttonName;
    this.displayName = eventDisplayName;
    this.buttonText = buttonText;
    this.actionPanelId = actionPanelId;
  }

  public String getDisplayName() {
    return displayName;
  }

  public String getCaseEventAction() {
    return caseEventAction;
  }

  public String getButtonText() {
    return buttonText;
  }

  public String getActionPanelId() {
    return actionPanelId;
  }
}
