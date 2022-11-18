package uk.co.nstauthority.scap.util;

import org.springframework.web.servlet.mvc.support.RedirectAttributes;
import uk.co.nstauthority.scap.fds.notificationbanner.NotificationBannerBodyLine;

public class DeletionSuccessBannerUtil {

  private DeletionSuccessBannerUtil() {
    throw new IllegalStateException("This is a helper class, it should not be instantiated");
  }

  public static void addRedirectionNotification(RedirectAttributes redirectAttributes, String message) {
    NotificationBannerUtils.successBannerRedirect(
        "Success",
        new NotificationBannerBodyLine(message, "govuk-!-font-weight-bold"),
        redirectAttributes);
  }
}
