package uk.co.nstauthority.scap.util;

import org.springframework.validation.BeanPropertyBindingResult;
import org.springframework.validation.BindingResult;

public class TaskListItemUtil {

  private TaskListItemUtil() {
  }

  public static BindingResult getBindingResultForForm(Object form) {
    return new BeanPropertyBindingResult(form, "form");
  }
}
