package uk.co.nstauthority.scap.fds.notificationbanner;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public class NotificationBannerView implements Serializable {
  private final String title;

  private final String heading;
  private final List<NotificationBannerBodyLine> bodyLines;
  private final NotificationBannerType bannerType;

  public static class BannerBuilder {
    private final String title;

    private String heading;
    private final List<NotificationBannerBodyLine> bodyLines;
    private final NotificationBannerType bannerType;

    public BannerBuilder(String title, NotificationBannerType bannerType) {
      this.title = title;
      this.heading = "";
      this.bodyLines = new ArrayList<>();
      this.bannerType = bannerType;
    }

    public BannerBuilder withHeading(String heading) {
      this.heading = heading;
      return this;
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
    this.heading = bannerBuilder.heading;
    this.bodyLines = bannerBuilder.bodyLines;
    this.bannerType = bannerBuilder.bannerType;
  }

  public String getTitle() {
    return title;
  }

  public String getHeading() {
    return heading;
  }

  public List<NotificationBannerBodyLine> getBodyLines() {
    return bodyLines;
  }

  public NotificationBannerType getBannerType() {
    return bannerType;
  }
}
