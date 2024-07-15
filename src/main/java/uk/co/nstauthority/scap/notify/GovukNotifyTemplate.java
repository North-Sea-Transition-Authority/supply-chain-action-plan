package uk.co.nstauthority.scap.notify;

public enum GovukNotifyTemplate {

  SCAP_SUBMISSION_NOTIFICATION("06be7163-4776-41bb-a810-13882ce7dcaf"),
  SCAP_APPROVAL_NOTIFICATION("084923e0-b8f5-4b46-98a0-985ff919be15"),
  SCAP_WITHDRAWAL_NOTIFICATION("6e1ec276-2b2e-4186-86be-83549f660499")
  ;

  private final String templateId;

  GovukNotifyTemplate(String templateId) {
    this.templateId = templateId;
  }

  public String getTemplateId() {
    return templateId;
  }
}
