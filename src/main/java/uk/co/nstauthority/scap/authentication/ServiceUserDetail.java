package uk.co.nstauthority.scap.authentication;

import java.io.Serializable;
import org.springframework.security.core.AuthenticatedPrincipal;
import uk.co.nstauthority.scap.energyportal.WebUserAccountId;

public record ServiceUserDetail(Long wuaId,
                                Long personId,
                                String forename,
                                String surname,
                                String emailAddress)
    implements AuthenticatedPrincipal, Serializable {

  @Override
  public String getName() {
    return wuaId.toString();
  }

  public String displayName() {
    return "%s %s".formatted(forename, surname);
  }

  public WebUserAccountId getWebUserAccountId() {
    return new WebUserAccountId(wuaId);
  }
}
