package uk.co.nstauthority.scap.permissionmanagement.endpointsecurity;

import java.util.Arrays;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Component;
import org.springframework.web.method.HandlerMethod;
import uk.co.nstauthority.scap.authentication.ServiceUserDetail;
import uk.co.nstauthority.scap.authentication.UserDetailService;
import uk.co.nstauthority.scap.error.exception.InvalidAuthenticationException;
import uk.co.nstauthority.scap.mvc.AbstractHandlerInterceptor;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberService;

@Component
public class PermissionManagementHandlerInterceptor extends AbstractHandlerInterceptor {

  private static final Logger LOGGER = LoggerFactory.getLogger(PermissionManagementHandlerInterceptor.class);

  private final TeamMemberService teamMemberService;

  private final UserDetailService userDetailService;

  @Autowired
  public PermissionManagementHandlerInterceptor(TeamMemberService teamMemberService,
                                                UserDetailService userDetailService) {
    this.teamMemberService = teamMemberService;
    this.userDetailService = userDetailService;
  }

  @Override
  public boolean preHandle(@NonNull HttpServletRequest request,
                           @NonNull HttpServletResponse response,
                           @NonNull Object handler) {

    if (handler instanceof HandlerMethod handlerMethod && hasAnnotation(handlerMethod, PermissionsRequired.class)) {
      var user = userDetailService.getUserDetail();
      checkUserHasPermission(
          user,
          ((PermissionsRequired) getAnnotation(handlerMethod, PermissionsRequired.class)).permissions());
    }
    return true;
  }

  private void checkUserHasPermission(ServiceUserDetail user, RolePermission[] permissions) {
    var userPermissions = teamMemberService.getAllPermissionsForUser(user);
    var hasPermission = Arrays.stream(permissions)
        .allMatch(userPermissions::contains);

    if (hasPermission) {
      return;
    }
    var errorMessage = "User with ID: %s does not have the required permissions."
        .formatted(user.wuaId());
    LOGGER.warn(errorMessage);
    throw new InvalidAuthenticationException(errorMessage);
  }
}