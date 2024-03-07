package uk.co.nstauthority.scap.utils;

import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.view;

import org.springframework.test.web.servlet.ResultMatcher;
import org.springframework.validation.BeanPropertyBindingResult;
import org.springframework.validation.BindingResult;
import org.springframework.validation.FieldError;

public class ControllerTestingUtil {

  private ControllerTestingUtil() {
    throw new IllegalStateException("ControllerTestingUtil is a util class and should not be instantiated");
  }

  public static ResultMatcher redirectUrl(String expectedRedirectUrl) {
    return view().name("redirect:%s".formatted(expectedRedirectUrl));
  }

  public static BindingResult bindingResultWithErrors(Object form) {
    var bindingResult = new BeanPropertyBindingResult(form, "form");
    bindingResult.addError(new FieldError("form", "testField", "Test error message"));
    return bindingResult;
  }
}
