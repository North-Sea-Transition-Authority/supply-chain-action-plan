package uk.co.nstauthority.scap.permissionmanagement.endpointsecurity;

import java.util.Collection;
import java.util.List;
import java.util.Map;
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
import org.springframework.web.servlet.HandlerMapping;
import uk.co.nstauthority.scap.authentication.ServiceUserDetail;
import uk.co.nstauthority.scap.authentication.UserDetailService;
import uk.co.nstauthority.scap.error.exception.InvalidAuthenticationException;
import uk.co.nstauthority.scap.mvc.AbstractHandlerInterceptor;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.permissionmanagement.TeamRole;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberService;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;
import uk.co.nstauthority.scap.scap.scap.Scap;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.scap.ScapService;

@Component
public class ScapPermissionManagementHandlerInterceptor extends AbstractHandlerInterceptor {

  private static final Logger LOGGER = LoggerFactory.getLogger(ScapPermissionManagementHandlerInterceptor.class);

  private final TeamMemberService teamMemberService;

  private final TeamService teamService;

  private final UserDetailService userDetailService;

  private final ScapService scapService;

  @Autowired
  public ScapPermissionManagementHandlerInterceptor(TeamMemberService teamMemberService,
                                                    TeamService teamService, UserDetailService userDetailService,
                                                    ScapService scapService) {
    this.teamMemberService = teamMemberService;
    this.teamService = teamService;
    this.userDetailService = userDetailService;
    this.scapService = scapService;
  }

  @Override
  public boolean preHandle(@NonNull HttpServletRequest request,
                           @NonNull HttpServletResponse response,
                           @NonNull Object handler) {

    if (handler instanceof HandlerMethod handlerMethod && hasAnnotation(handlerMethod, PermissionsRequiredForScap.class)) {
      var scap = extractScapFromRequest(request, handlerMethod);
      var user = userDetailService.getUserDetail();

      var permissionsRequired =
          ((PermissionsRequiredForScap) getAnnotation(handlerMethod, PermissionsRequiredForScap.class)).permissions();
      checkIsMemberOfScapTeamWithPermission(scap, user, List.of(permissionsRequired));
    }
    return true;
  }

  private void checkIsMemberOfScapTeamWithPermission(Scap scap, ServiceUserDetail user, List<RolePermission> permissions) {
    if (teamService.userIsMemberOfRegulatorTeam(user)) {
      return;
    }
    var team = teamService.getByEnergyPortalOrgGroupId(scap.getOrganisationGroupId());
    var teamMember = teamMemberService.getTeamMember(team, user.getWebUserAccountId());

    var teamMemberPermissions = teamMember.roles()
        .stream()
        .map(TeamRole::getRolePermissions)
        .flatMap(Collection::stream)
        .anyMatch(permissions::contains);
    if (teamMemberPermissions) {
      return;
    }
    throw new InvalidAuthenticationException(
        "User ID: %s does not have permission to view page".formatted(user.wuaId()));
  }

  private Scap extractScapFromRequest(HttpServletRequest httpServletRequest, HandlerMethod handlerMethod) {

    var scapIdParameter = getPathVariableByClass(handlerMethod, ScapId.class);

    if (scapIdParameter.isEmpty()) {
      var errorMessage = "No path variable called scapId found in request";
      throw new ResponseStatusException(HttpStatus.BAD_REQUEST, errorMessage);
    }

    var pathVariables = (Map<String, String>) httpServletRequest
        .getAttribute(HandlerMapping.URI_TEMPLATE_VARIABLES_ATTRIBUTE);

    return scapService.getScapById(Integer.valueOf(pathVariables.get(scapIdParameter.get().getName())));
  }
}