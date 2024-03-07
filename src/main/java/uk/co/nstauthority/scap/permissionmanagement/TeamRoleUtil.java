package uk.co.nstauthority.scap.permissionmanagement;

import java.util.Collection;
import java.util.Set;
import java.util.stream.Collectors;
import uk.co.nstauthority.scap.error.exception.IllegalUtilClassInstantiationException;

public class TeamRoleUtil {

  private TeamRoleUtil() {
    throw new IllegalUtilClassInstantiationException(this.getClass());
  }

  public static Set<String> getRoleNames(Collection<TeamRole> roles) {
    return roles.stream()
        .map(TeamRole::getEnumName)
        .collect(Collectors.toSet());
  }

}
