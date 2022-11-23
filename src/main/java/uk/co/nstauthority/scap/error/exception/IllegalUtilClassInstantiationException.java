package uk.co.nstauthority.scap.error.exception;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

@ResponseStatus(value = HttpStatus.INTERNAL_SERVER_ERROR, reason = "Class cannot be Instantiated")
public class IllegalUtilClassInstantiationException extends IllegalStateException {

  public IllegalUtilClassInstantiationException(Class<?> utilClass) {
    super("%s is a util class and should not be instantiated".formatted(utilClass.getName()));
  }
}
