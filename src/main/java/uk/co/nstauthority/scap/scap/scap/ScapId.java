package uk.co.nstauthority.scap.scap.scap;

import org.apache.commons.lang3.math.NumberUtils;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ResponseStatusException;

public record ScapId(Integer scapId) {

  public static ScapId valueOf(String value) {
    if (NumberUtils.isDigits(value)) {
      return new ScapId(Integer.valueOf(value));
    }
    throw new ResponseStatusException(
        HttpStatus.NOT_FOUND,
        String.format("Cannot find Scap with ID: %s", value)
    );
  }

  public static ScapId valueOf(Integer value) {
    return new ScapId(value);
  }

  @Override
  public String toString() {
    return String.valueOf(scapId);
  }
}
