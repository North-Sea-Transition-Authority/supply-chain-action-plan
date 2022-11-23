package uk.co.nstauthority.scap.error.exception;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

@ResponseStatus(value = HttpStatus.UNAUTHORIZED, reason = "Authentication Failed")
public class SamlResponseException extends RuntimeException {

  public SamlResponseException(String message) {
    super(message);
  }

}
