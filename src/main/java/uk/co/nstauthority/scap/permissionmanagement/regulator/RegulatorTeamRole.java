package uk.co.nstauthority.scap.permissionmanagement.regulator;

import java.util.EnumSet;
import java.util.Optional;
import java.util.Set;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.permissionmanagement.TeamRole;

public enum RegulatorTeamRole implements TeamRole {

  ACCESS_MANAGER(
      "Access manager",
      "Can add, remove and update members of this team",
      10,
      EnumSet.of(RolePermission.GRANT_ROLES)
  ),
  ORGANISATION_ACCESS_MANAGER(
      "Organisation access manager",
      "Can manage organisation access to this service",
      20,
      EnumSet.of(RolePermission.MANAGE_ORGANISATIONS)
  ),
  SCAP_CASE_OFFICER(
      "Case officer",
      "Can process SCAP applications",
      30,
      EnumSet.of(RolePermission.REVIEW_SCAP, RolePermission.VIEW_SCAP)
  ),
  SCAP_VIEWER(
      "SCAP viewer",
      "Can view SCAP applications",
      40,
      EnumSet.of(RolePermission.VIEW_SCAP)
  );

  private final String screenDisplayText;
  private final String description;
  private final Integer displayOrder;
  private final Set<RolePermission> rolePermissions;

  RegulatorTeamRole(String screenDisplayText, String description, Integer displayOrder,
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

  static Optional<RegulatorTeamRole> getRoleFromString(String role) {
    try {
      return Optional.of(RegulatorTeamRole.valueOf(role.toUpperCase()));
    } catch (IllegalArgumentException exception) {
      return Optional.empty();
    }
  }


}
