package uk.co.nstauthority.scap.utils;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import org.springframework.validation.BindingResult;
import org.springframework.validation.FieldError;

public class ValidatorTestingUtil {

  private ValidatorTestingUtil() {
    throw new IllegalStateException("ValidatorTestingUtil is a util class and should not be instantiated");
  }

  /**
   * Return a map of field id -> set of field error codes for a BindingResult.
   */
  public static Map<String, Set<String>> extractErrors(BindingResult bindingResult) {

    return bindingResult.getFieldErrors().stream()
        .collect(Collectors.groupingBy(
            FieldError::getField,
            LinkedHashMap::new,
            Collectors.mapping(FieldError::getCode, Collectors.toSet())
        ));

  }

  public static class NonSupportedClass {

  }
}
