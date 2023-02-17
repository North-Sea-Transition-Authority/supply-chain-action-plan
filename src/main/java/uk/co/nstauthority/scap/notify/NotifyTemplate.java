package uk.co.nstauthority.scap.notify;

public enum NotifyTemplate {

  EMAIL_DELIVERY_FAILED("EMAIL_DELIVERY_FAILED_V1"),
  SCAP_APPROVED("SCAP Approval Notification");

  private final String templateName;

  NotifyTemplate(String templateName) {
    this.templateName = templateName;
  }

  public String getTemplateName() {
    return this.templateName;
  }
}
