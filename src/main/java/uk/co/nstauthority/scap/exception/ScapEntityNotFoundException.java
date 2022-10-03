package uk.co.nstauthority.scap.exception;

import javax.persistence.EntityNotFoundException;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

@ResponseStatus(value = HttpStatus.NOT_FOUND, reason = "The item could not be found")
public class ScapEntityNotFoundException extends EntityNotFoundException {
  public ScapEntityNotFoundException(String message) {
    super(message);
  }
}
