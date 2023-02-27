package uk.co.nstauthority.scap.scap.casemanagement;



import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventAction.APPROVED;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventAction.CLOSED_OUT;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventAction.CONSULTATION_REQUESTED;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventAction.CONSULTATION_RESPONSE;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventAction.INFO_REQUESTED;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventAction.INFO_RESPONSE;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventAction.QA;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventAction.SUBMIT;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventAction.UPDATE;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventAction.WITHDRAWN;

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
      "Further Information Response",
      "Info-Response-Panel"),
  SCAP_SUBMITTED(SUBMIT,
      "Submitted SCAP",
      null,
      null),
  UPDATE_SCAP(UPDATE,
      "SCAP UPDATE",
      null,
      "Update-Panel"),
  SCAP_CLOSED_OUT(
      CLOSED_OUT,
      "Project completed",
      null,
      null),
  SCAP_CONSULTATION_REQUESTED(CONSULTATION_REQUESTED,
      "Consultation Requested",
      "Request Consultation",
      "Consultation-Request-Panel"),
  SCAP_CONSULTATION_RESPONSE(CONSULTATION_RESPONSE,
      "Consultation Response",
      "Consultation Response",
      "Consultation-Response-Panel"),
  SCAP_APPROVED(APPROVED,
      "SCAP Approved",
      "Approve SCAP",
      "Approve-scap-Panel"),
  SCAP_WITHDRAWN(WITHDRAWN,
      "SCAP Withdrawn",
      "Withdraw SCAP",
      "Withdraw-scap-panel");

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
