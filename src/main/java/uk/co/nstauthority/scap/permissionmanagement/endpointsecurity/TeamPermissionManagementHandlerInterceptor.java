package uk.co.nstauthority.scap.permissionmanagement.endpointsecurity;

import java.util.Arrays;
import java.util.Collection;
import java.util.Map;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Component;
import org.springframework.web.method.HandlerMethod;
import org.springframework.web.servlet.HandlerMapping;
import uk.co.nstauthority.scap.authentication.ServiceUserDetail;
import uk.co.nstauthority.scap.authentication.UserDetailService;
import uk.co.nstauthority.scap.energyportal.WebUserAccountId;
import uk.co.nstauthority.scap.error.exception.InvalidAuthenticationException;
import uk.co.nstauthority.scap.error.exception.ScapBadRequestException;
import uk.co.nstauthority.scap.mvc.AbstractHandlerInterceptor;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.permissionmanagement.TeamId;
import uk.co.nstauthority.scap.permissionmanagement.TeamRole;
import uk.co.nstauthority.scap.permissionmanagement.TeamType;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberService;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;

@Component
public class TeamPermissionManagementHandlerInterceptor extends AbstractHandlerInterceptor {

  private static final Logger LOGGER = LoggerFactory.getLogger(TeamPermissionManagementHandlerInterceptor.class);

  private final TeamMemberService teamMemberService;

  private final TeamService teamService;

  private final UserDetailService userDetailService;

  @Autowired
  public TeamPermissionManagementHandlerInterceptor(TeamMemberService teamMemberService,
                                                    TeamService teamService, UserDetailService userDetailService) {
    this.teamMemberService = teamMemberService;
    this.teamService = teamService;
    this.userDetailService = userDetailService;
  }

  @Override
  public boolean preHandle(@NonNull HttpServletRequest request,
                           @NonNull HttpServletResponse response,
                           @NonNull Object handler) {

    if (handler instanceof HandlerMethod handlerMethod && hasAnnotation(handlerMethod, PermissionsRequiredForTeam.class)) {
      var teamId = extractTeamIdFromRequest(request, handlerMethod);
      var user = userDetailService.getUserDetail();
      checkIsMemberOfTeamWithRole(
          teamId,
          user,
          ((PermissionsRequiredForTeam) getAnnotation(handlerMethod, PermissionsRequiredForTeam.class)).permissions());
    }
    return true;
  }

  private void checkIsMemberOfTeamWithRole(TeamId teamId, ServiceUserDetail user, RolePermission[] permissions) {

    if (partOfRegulatorTeamWithPermission(user, permissions)
        || partOfIndustryTeamWithPermission(user, teamId, permissions)) {
      return;
    }
    var errorMessage = "User with ID: %s does not belong to team with ID: %s"
        .formatted(user.wuaId(), teamId.uuid());
    LOGGER.warn(errorMessage);
    throw new InvalidAuthenticationException(errorMessage);
  }

  /**
   * Checks if user is part of regulator team and has relevant permission.
   * @param user the user to check.
   * @return if they have the correct permission and are in the regulator team.
   */
  private boolean partOfRegulatorTeamWithPermission(ServiceUserDetail user, RolePermission[] permissions) {
    var webUserAccountId = new WebUserAccountId(user.wuaId());
    var regulatorTeams = teamService.getTeamsOfTypeThatUserBelongsTo(user, TeamType.REGULATOR);
    if (!regulatorTeams.isEmpty()) {
      var teamMember = teamMemberService.findTeamMember(regulatorTeams.get(0), webUserAccountId);
      if (teamMember.isPresent()) {
        var teamMemberPermissions = teamMember.get()
            .roles()
            .stream()
            .map(TeamRole::getRolePermissions)
            .flatMap(Collection::stream)
            .toList();

        if (Arrays.stream(permissions).anyMatch(teamMemberPermissions::contains)) {
          return true;
        }
      }
    }
    return false;
  }

  private boolean partOfIndustryTeamWithPermission(ServiceUserDetail user, TeamId teamId, RolePermission[] permissions) {
    // Get roles associated with that User and Team only.
    // Roles assigned in other non-Regulator teams have no effect.
    var webUserAccountId = new WebUserAccountId(user.wuaId());
    var industryTeam = teamService.getTeam(teamId);
    var teamMember = teamMemberService.findTeamMember(industryTeam, webUserAccountId);
    if (teamMember.isPresent()) {
      var teamMemberPermissions = teamMember.get()
          .roles()
          .stream()
          .map(TeamRole::getRolePermissions)
          .flatMap(Collection::stream)
          .toList();

      if (Arrays.stream(permissions).anyMatch(teamMemberPermissions::contains)) {
        return true;
      }
      var errorMessage = "User with ID: %s does not have any roles with the specified permissions for team with ID: %s"
          .formatted(user.wuaId(), teamId.uuid());
      LOGGER.warn(errorMessage);
      throw new InvalidAuthenticationException(errorMessage);
    }
    return false;
  }

  public static TeamId extractTeamIdFromRequest(HttpServletRequest httpServletRequest, HandlerMethod handlerMethod) {
    var teamIdParameter = getPathVariableByClass(handlerMethod, TeamId.class);
    if (teamIdParameter.isEmpty()) {
      throw new ScapBadRequestException("TeamId is not included in parameters");
    }

    @SuppressWarnings("unchecked")
    var pathVariables = (Map<String, String>) httpServletRequest
        .getAttribute(HandlerMapping.URI_TEMPLATE_VARIABLES_ATTRIBUTE);

    return TeamId.valueOf(pathVariables.get(teamIdParameter.get().getName()));
  }
}
