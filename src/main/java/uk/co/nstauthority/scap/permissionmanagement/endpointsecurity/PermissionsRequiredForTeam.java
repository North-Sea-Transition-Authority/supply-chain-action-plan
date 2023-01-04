package uk.co.nstauthority.scap.permissionmanagement.endpointsecurity;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;

@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.METHOD, ElementType.TYPE})
/* Restricts the endpoint to users who have the required permissions for the team specified in the URL.
 * The currently logged-in user has their permissions from only the specified Team verified.
 * They are given access to the endpoint if they have the permission in the team the endpoint is accessing. */
public @interface PermissionsRequiredForTeam {

  RolePermission[] permissions();
}
