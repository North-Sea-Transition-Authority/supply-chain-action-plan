package uk.co.nstauthority.scap.fds.notificationbanner;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public class NotificationBannerView implements Serializable {
  private final String title;
  private final List<NotificationBannerBodyLine> bodyLines;
  private final NotificationBannerType bannerType;

  public static class BannerBuilder {
    private final String title;
    private final List<NotificationBannerBodyLine> bodyLines;
    private final NotificationBannerType bannerType;

    public BannerBuilder(String title, NotificationBannerType bannerType) {
      this.title = title;
      this.bodyLines = new ArrayList<>();
      this.bannerType = bannerType;
    }

    public BannerBuilder addBodyLine(NotificationBannerBodyLine notificationBannerBodyLine) {
      this.bodyLines.add(notificationBannerBodyLine);
      return this;
    }

    public BannerBuilder addBodyLines(List<NotificationBannerBodyLine> notificationBannerBodyLines) {
      this.bodyLines.addAll(notificationBannerBodyLines);
      return this;
    }

    public NotificationBannerView build() {
      return new NotificationBannerView(this);
    }

  }

  private NotificationBannerView(BannerBuilder bannerBuilder) {
    this.title = bannerBuilder.title;
    this.bodyLines = bannerBuilder.bodyLines;
    this.bannerType = bannerBuilder.bannerType;
  }

  public String getTitle() {
    return title;
  }

  public List<NotificationBannerBodyLine> getBodyLines() {
    return bodyLines;
  }

  public NotificationBannerType getBannerType() {
    return bannerType;
  }
}
