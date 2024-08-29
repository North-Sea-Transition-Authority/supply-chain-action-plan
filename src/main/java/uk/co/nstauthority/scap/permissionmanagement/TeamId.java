package uk.co.nstauthority.scap.permissionmanagement;

import java.util.UUID;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ResponseStatusException;

public record TeamId(UUID uuid) {

  public static TeamId valueOf(UUID value) {
    return new TeamId(value);
  }

  // Required for Spring converter mapping
  public static TeamId valueOf(String value) {
    try {
      return new TeamId(UUID.fromString(value));
    } catch (Exception e) {
      throw new ResponseStatusException(
          HttpStatus.NOT_FOUND,
          String.format("Cannot find Team with ID: %s", value)
      );
    }
  }

  @Override
  public String toString() {
    return uuid.toString();
  }
}
