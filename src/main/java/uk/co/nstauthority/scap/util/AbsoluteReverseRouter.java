package uk.co.nstauthority.scap.util;

import org.springframework.web.servlet.support.ServletUriComponentsBuilder;
import uk.co.nstauthority.scap.mvc.ReverseRouter;

public class AbsoluteReverseRouter {

  private AbsoluteReverseRouter() {
  }

  public static String route(Object methodCall) {
    return getContextPath() + ReverseRouter.route(methodCall);
  }

  private static String getContextPath() {
    return ServletUriComponentsBuilder.fromCurrentContextPath().toUriString();
  }
}
