package uk.co.nstauthority.scap.scap.casemanagement;


import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventAction.APPROVED;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventAction.CLOSED_OUT;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventAction.CONSULTATION_REQUESTED;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventAction.CONSULTATION_RESPONSE;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventAction.INFO_REQUESTED;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventAction.INFO_RESPONSE;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventAction.QA;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventAction.REINSTATE;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventAction.SUBMIT;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventAction.UPDATE_REQUESTED;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventAction.UPDATE_SUBMITTED;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventAction.WITHDRAWN;

public enum CaseEventSubject {
  QA_COMMENT(QA,
      "QA checks completed",
      "Complete QA checks",
      "Qa-Panel"),
  FURTHER_INFO_REQUESTED(INFO_REQUESTED,
      "Additional information requested",
      "Request further information",
      "Info-Request-Panel"),
  FURTHER_INFO_RESPONSE(INFO_RESPONSE,
      "Additional information provided",
      "Further information response",
      "Info-Response-Panel"),
  SCAP_SUBMITTED(SUBMIT,
      "Submitted SCAP",
      null,
      null),
  UPDATE_SCAP(UPDATE_SUBMITTED,
      "SCAP updated",
      null,
      "Update-Panel"),
  SCAP_CLOSED_OUT(
      CLOSED_OUT,
      "Project completed",
      null,
      null),
  SCAP_CONSULTATION_REQUESTED(CONSULTATION_REQUESTED,
      "Consultation requested",
      "Request Consultation",
      "Consultation-Request-Panel"),
  SCAP_CONSULTATION_RESPONSE(CONSULTATION_RESPONSE,
      "Consultation response provided",
      "Provide consultation response",
      "Consultation-Response-Panel"),
  SCAP_UPDATE_REQUESTED(UPDATE_REQUESTED,
      "Required update set",
      "Set next required update",
      "Update-Request-Panel"),
  SCAP_APPROVED(APPROVED,
      "No objection",
      "No objection",
      "No-Objection-scap-Panel"),
  SCAP_WITHDRAWN(WITHDRAWN,
      "SCAP withdrawn",
      "Withdraw SCAP",
      "Withdraw-scap-panel"),
  SCAP_REINSTATED(REINSTATE,
      "SCAP reinstated",
      "Reinstate SCAP",
      "Reinstate-scap-panel");

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
