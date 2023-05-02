package uk.co.nstauthority.scap.endpointvalidation.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;

@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD})
public @interface HasAnyPermissionForScap {
  boolean allowRegulatorAccess() default false;
  RolePermission[] permissions();
}
