package uk.co.nstauthority.scap.util;

import org.springframework.web.servlet.mvc.support.RedirectAttributes;
import uk.co.nstauthority.scap.error.exception.IllegalUtilClassInstantiationException;
import uk.co.nstauthority.scap.fds.notificationbanner.NotificationBannerBodyLine;

public class SuccessBannerUtil {

  private SuccessBannerUtil() {
    throw new IllegalUtilClassInstantiationException(SuccessBannerUtil.class);
  }

  public static void add(RedirectAttributes redirectAttributes, String message) {
    NotificationBannerUtils.successBannerRedirect(
        "Success",
        new NotificationBannerBodyLine(message, "govuk-!-font-weight-bold"),
        redirectAttributes);
  }
}
