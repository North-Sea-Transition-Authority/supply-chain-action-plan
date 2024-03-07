package uk.co.nstauthority.scap.fds.notificationbanner;

import java.io.Serializable;

public record NotificationBannerBodyLine(String lineText, String lineClass) implements Serializable {
}
