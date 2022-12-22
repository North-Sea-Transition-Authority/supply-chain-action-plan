package uk.co.nstauthority.scap.permissionmanagement;

import java.util.Arrays;
import java.util.Collection;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Component;
import org.springframework.web.method.HandlerMethod;
import org.springframework.web.server.ResponseStatusException;
import uk.co.nstauthority.scap.authentication.ServiceUserDetail;
import uk.co.nstauthority.scap.authentication.UserDetailService;
import uk.co.nstauthority.scap.energyportal.WebUserAccountId;
import uk.co.nstauthority.scap.mvc.AbstractHandlerInterceptor;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberService;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;

@Component
public class PermissionManagementHandlerInterceptor extends AbstractHandlerInterceptor {

  private static final Logger LOGGER = LoggerFactory.getLogger(PermissionManagementHandlerInterceptor.class);

  private final TeamMemberService teamMemberService;

  private final TeamService teamService;

  private final UserDetailService userDetailService;

  @Autowired
  public PermissionManagementHandlerInterceptor(TeamMemberService teamMemberService,
                                                TeamService teamService, UserDetailService userDetailService) {
    this.teamMemberService = teamMemberService;
    this.teamService = teamService;
    this.userDetailService = userDetailService;
  }

  @Override
  public boolean preHandle(@NonNull HttpServletRequest request,
                           @NonNull HttpServletResponse response,
                           @NonNull Object handler) {

    if (handler instanceof HandlerMethod handlerMethod && hasAnnotation(handlerMethod, PermissionsRequired.class)) {
      var teamId = TeamManagementHandlerInterceptor.extractTeamIdFromRequest(request, handlerMethod);
      var user = userDetailService.getUserDetail();
      checkIsMemberOfTeamWithRole(
          teamId,
          user,
          ((PermissionsRequired) getAnnotation(handlerMethod, PermissionsRequired.class)).permissions());
    }
    return true;
  }

  private void checkIsMemberOfTeamWithRole(TeamId teamId, ServiceUserDetail user, RolePermission[] permissions) {
    var webUserAccountId = new WebUserAccountId(user.wuaId());

    var industryTeam = teamService.getTeam(teamId);
    var regulatorTeams = teamService.getTeamsOfTypeThatUserBelongsTo(user, TeamType.REGULATOR);

    // If Regulator Team Member has relevant permission,
    // they can act as if Industry Member of that team with that permission.
    if (!regulatorTeams.isEmpty()) {
      var teamMember = teamMemberService.getTeamMember(regulatorTeams.get(0), webUserAccountId);
      if (teamMember.isPresent()) {
        var teamMemberPermissions = teamMember.get()
            .roles()
            .stream()
            .map(TeamRole::getRolePermissions)
            .flatMap(Collection::stream)
            .toList();

        if (Arrays.stream(permissions).anyMatch(teamMemberPermissions::contains)) {
          return;
        }
      }
    }

    // Get roles associated with that User and Team only.
    // Roles assigned in other non-Regulator teams have no effect.
    var teamMember = teamMemberService.getTeamMember(industryTeam, webUserAccountId);
    if (teamMember.isPresent()) {
      var teamMemberPermissions = teamMember.get()
          .roles()
          .stream()
          .map(TeamRole::getRolePermissions)
          .flatMap(Collection::stream)
          .toList();

      if (Arrays.stream(permissions).anyMatch(teamMemberPermissions::contains)) {
        return;
      }
      var errorMessage = "User with ID: %s does not have any roles with the specified permissions for team with ID: %s"
          .formatted(user.wuaId(), teamId.uuid());
      LOGGER.warn(errorMessage);
      throw new ResponseStatusException(HttpStatus.FORBIDDEN, errorMessage);
    }
    var errorMessage = "User with ID: %s does not belong to team with ID: %s"
        .formatted(user.wuaId(), teamId.uuid());
    LOGGER.warn(errorMessage);
    throw new ResponseStatusException(HttpStatus.FORBIDDEN, errorMessage);
  }
}