package uk.co.nstauthority.scap.energyportal;

import java.io.Serial;
import java.io.Serializable;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ResponseStatusException;

public record WebUserAccountId(Long id) implements Serializable {

  @Serial
  private static final long serialVersionUID = 5765464679246137275L;

  public int toInt() {
    return id.intValue();
  }

  public String toString() {
    return String.valueOf(id);
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    WebUserAccountId that = (WebUserAccountId) o;
    return id.equals(that.id);
  }

  public static WebUserAccountId valueOf(String webUserAccountId) {
    try {
      return new WebUserAccountId(Long.valueOf(webUserAccountId));
    } catch (Exception e) {
      throw new ResponseStatusException(
          HttpStatus.NOT_FOUND,
          String.format("Cannot find WebUserAccount with ID: %s", webUserAccountId)
      );
    }
  }
}
