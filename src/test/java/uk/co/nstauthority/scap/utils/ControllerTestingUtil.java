package uk.co.nstauthority.scap.utils;

import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.view;

import org.springframework.test.web.servlet.ResultMatcher;

public class ControllerTestingUtil {

  private ControllerTestingUtil() {
    throw new IllegalStateException("ControllerTestingUtil is a util class and should not be instantiated");
  }

  public static ResultMatcher redirectUrl(String expectedRedirectUrl) {
    return view().name("redirect:%s".formatted(expectedRedirectUrl));
  }
}
