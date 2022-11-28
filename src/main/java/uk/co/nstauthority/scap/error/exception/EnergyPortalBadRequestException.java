package uk.co.nstauthority.scap.error.exception;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

@ResponseStatus(value = HttpStatus.BAD_REQUEST, reason = "Energy Portal User not allowed")
public class EnergyPortalBadRequestException extends RuntimeException {
  public EnergyPortalBadRequestException(String message) {
    super(message);
  }
}
