package uk.co.nstauthority.scap.fds.notificationbanner;

public enum NotificationBannerType {
  INFO("notificationBannerInfo"),
  SUCCESS("notificationBannerSuccess");

  private final String value;

  NotificationBannerType(String value) {
    this.value = value;
  }

  public String getValue() {
    return value;
  }
}
