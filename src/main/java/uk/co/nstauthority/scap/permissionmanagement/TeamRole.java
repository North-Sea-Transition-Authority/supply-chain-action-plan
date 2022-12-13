package uk.co.nstauthority.scap.permissionmanagement;

import java.util.Set;
import uk.co.nstauthority.scap.enumutil.DisplayableEnumOption;

public interface TeamRole extends DisplayableEnumOption {

  Set<RolePermission> getRolePermissions();

  String name();
}
