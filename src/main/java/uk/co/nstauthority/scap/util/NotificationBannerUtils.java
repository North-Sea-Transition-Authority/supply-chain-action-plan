package uk.co.nstauthority.scap.util;

import java.util.List;
import org.springframework.web.servlet.ModelAndView;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;
import uk.co.nstauthority.scap.error.exception.IllegalUtilClassInstantiationException;
import uk.co.nstauthority.scap.fds.notificationbanner.NotificationBannerBodyLine;
import uk.co.nstauthority.scap.fds.notificationbanner.NotificationBannerType;
import uk.co.nstauthority.scap.fds.notificationbanner.NotificationBannerView;

public class NotificationBannerUtils {

  public static final String NOTIFICATION_BANNER_OBJECT_NAME = "notificationBannerView";

  private NotificationBannerUtils() {
    throw new IllegalUtilClassInstantiationException(NotificationBannerUtils.class);
  }

  public static void successBanner(String header,
                                   List<NotificationBannerBodyLine> bodyLines,
                                   ModelAndView modelAndView) {
    var notificationBannerView = new NotificationBannerView.BannerBuilder("Success", NotificationBannerType.SUCCESS)
        .withHeading(header)
        .addBodyLines(bodyLines)
        .build();
    modelAndView.addObject(NOTIFICATION_BANNER_OBJECT_NAME, notificationBannerView);
  }

  public static void successBannerRedirect(String header,
                                           List<NotificationBannerBodyLine> bodyLine,
                                           RedirectAttributes redirectAttributes) {
    var notificationBannerView = new NotificationBannerView.BannerBuilder("Success", NotificationBannerType.SUCCESS)
        .withHeading(header)
        .addBodyLines(bodyLine)
        .build();
    redirectAttributes.addFlashAttribute(NOTIFICATION_BANNER_OBJECT_NAME, notificationBannerView);
  }
}
