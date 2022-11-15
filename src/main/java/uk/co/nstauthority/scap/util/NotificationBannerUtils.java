package uk.co.nstauthority.scap.util;

import java.util.List;
import org.springframework.web.servlet.ModelAndView;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;
import uk.co.nstauthority.scap.fds.notificationbanner.NotificationBannerBodyLine;
import uk.co.nstauthority.scap.fds.notificationbanner.NotificationBannerType;
import uk.co.nstauthority.scap.fds.notificationbanner.NotificationBannerView;

public class NotificationBannerUtils {

  private static final String NOTIFICATION_BANNER_OBJECT_NAME = "notificationBannerView";

  private NotificationBannerUtils() {
    throw new IllegalStateException("This is a helper class, it should not be instantiated");
  }

  public static void successBanner(String title, List<NotificationBannerBodyLine> bodyLines, ModelAndView modelAndView) {
    var notificationBannerView = new NotificationBannerView.BannerBuilder(title, NotificationBannerType.SUCCESS)
        .addBodyLines(bodyLines)
        .build();
    modelAndView.addObject(NOTIFICATION_BANNER_OBJECT_NAME, notificationBannerView);
  }

  public static void successBanner(String title, NotificationBannerBodyLine bodyLine, ModelAndView modelAndView) {
    var notificationBannerView = new NotificationBannerView.BannerBuilder(title, NotificationBannerType.SUCCESS)
        .addBodyLine(bodyLine)
        .build();
    modelAndView.addObject(NOTIFICATION_BANNER_OBJECT_NAME, notificationBannerView);
  }

  public static void successBannerRedirect(String title,
                                           NotificationBannerBodyLine bodyLine,
                                           RedirectAttributes redirectAttributes) {
    var notificationBannerView = new NotificationBannerView.BannerBuilder(title, NotificationBannerType.SUCCESS)
        .addBodyLine(bodyLine)
        .build();
    redirectAttributes.addFlashAttribute(NOTIFICATION_BANNER_OBJECT_NAME, notificationBannerView);
  }

}
