package uk.co.nstauthority.scap.energyportal;

import java.io.Serial;
import java.io.Serializable;

public record WebUserAccountId(Long id) implements Serializable {

  @Serial
  private static final long serialVersionUID = 5765464679246137275L;

  public int toInt() {
    return id.intValue();
  }

  public String toString() {
    return String.valueOf(id);
  }

  public static WebUserAccountId valueOf(String webUserAccountId) {
    return new WebUserAccountId(Long.valueOf(webUserAccountId));
  }
}
