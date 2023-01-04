package uk.co.nstauthority.scap.scap.scap;

import java.util.UUID;
import uk.co.nstauthority.scap.permissionmanagement.TeamId;

public record ScapId(Integer scapId) {

  public static ScapId valueOf(String value) {
    return new ScapId(Integer.valueOf(value));
  }

  @Override
  public String toString() {
    return String.valueOf(scapId);
  }
}
