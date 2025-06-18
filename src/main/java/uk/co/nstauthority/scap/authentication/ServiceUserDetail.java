package uk.co.nstauthority.scap.authentication;

import java.io.Serializable;
import java.util.Objects;
import org.springframework.security.core.AuthenticatedPrincipal;
import uk.co.nstauthority.scap.energyportal.WebUserAccountId;

public record ServiceUserDetail(Long wuaId,
                                Long personId,
                                String forename,
                                String surname,
                                String emailAddress,
                                Long proxyWuaId,
                                String proxyUserDisplayName)
    implements AuthenticatedPrincipal, Serializable {

  @Override
  public String getName() {
    return Objects.nonNull(proxyWuaId) ? proxyWuaId.toString() : wuaId.toString();
  }

  public String displayName() {
    var userName = String.format("%s %s", forename, surname);
    return Objects.nonNull(proxyUserDisplayName) ? String.format("%s/%s", proxyUserDisplayName, userName) : userName;
  }

  public WebUserAccountId getWebUserAccountId() {
    return new WebUserAccountId(wuaId);
  }
}
