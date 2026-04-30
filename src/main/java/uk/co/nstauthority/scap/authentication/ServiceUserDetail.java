package uk.co.nstauthority.scap.authentication;

import java.io.Serializable;
import java.util.Objects;
import org.springframework.security.core.AuthenticatedPrincipal;
import uk.co.nstauthority.scap.energyportal.EnergyPortalUserDto;
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
    var displayName = String.format("%s %s", forename, surname);
    return Objects.nonNull(proxyUserDisplayName) ? String.format("%s as %s", proxyUserDisplayName, displayName) : displayName;
  }

  public WebUserAccountId getWebUserAccountId() {
    return new WebUserAccountId(wuaId);
  }

  public static ServiceUserDetail from(EnergyPortalUserDto energyPortalUser) {
    return new ServiceUserDetail(
        energyPortalUser.webUserAccountId(),
        null,
        energyPortalUser.forename(),
        energyPortalUser.surname(),
        energyPortalUser.emailAddress(),
        null,
        null
    );
  }
}
