package uk.co.nstauthority.scap.permissionmanagement.endpointsecurity;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;

@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.METHOD, ElementType.TYPE})
/* Restricts endpoint to users who have the required permissions for the team related to the SCAP specified in the URL.
 * The currently logged-in user has their permissions from only the specified Team.
 * They are given access to the endpoint if they have the permission in the team who owns the SCAP being accessed. */
public @interface PermissionsRequiredForScap {

  RolePermission[] permissions();
}
