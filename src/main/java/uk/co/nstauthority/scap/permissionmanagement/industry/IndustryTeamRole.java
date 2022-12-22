package uk.co.nstauthority.scap.permissionmanagement.industry;

import java.util.EnumSet;
import java.util.Optional;
import java.util.Set;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.permissionmanagement.TeamRole;

public enum IndustryTeamRole implements TeamRole {

  ACCESS_MANAGER(
      "Access manager",
      "Can add, remove and update members of this team",
      10,
      EnumSet.of(RolePermission.MANAGE_ORGANISATIONS)
  ),
  SCAP_SUBMITTER(
      "SCAP submitter",
      "Can create or update scaps",
      30,
      EnumSet.of(RolePermission.SUBMIT_SCAP)
  ),
  SCAP_VIEWER(
      "SCAP viewer",
      "can view an open SCAP",
      40,
      EnumSet.of(RolePermission.VIEW_SCAP)
  );

  private final String screenDisplayText;
  private final String description;
  private final Integer displayOrder;
  private final Set<RolePermission> rolePermissions;

  IndustryTeamRole(String screenDisplayText, String description, Integer displayOrder,
                   Set<RolePermission> rolePermissions) {
    this.screenDisplayText = screenDisplayText;
    this.description = description;
    this.displayOrder = displayOrder;
    this.rolePermissions = rolePermissions;
  }

  @Override
  public String getDescription() {
    return description;
  }

  @Override
  public String getDisplayName() {
    return screenDisplayText;
  }

  @Override
  public int getDisplayOrder() {
    return displayOrder;
  }

  @Override
  public String getFormValue() {
    return null;
  }

  @Override
  public String getEnumName() {
    return name();
  }

  @Override
  public Set<RolePermission> getRolePermissions() {
    return rolePermissions;
  }

  static Optional<IndustryTeamRole> getRoleFromString(String role) {
    try {
      return Optional.of(IndustryTeamRole.valueOf(role.toUpperCase()));
    } catch (IllegalArgumentException exception) {
      return Optional.empty();
    }
  }
}
