package uk.co.nstauthority.scap.permissionmanagement.endpointsecurity;

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
import uk.co.nstauthority.scap.mvc.AbstractHandlerInterceptor;
import uk.co.nstauthority.scap.permissionmanagement.TeamId;
import uk.co.nstauthority.scap.permissionmanagement.TeamType;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberService;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;

@Component
public class TeamManagementHandlerInterceptor extends AbstractHandlerInterceptor {

  private static final Logger LOGGER = LoggerFactory.getLogger(TeamManagementHandlerInterceptor.class);

  private final TeamService teamService;

  private final UserDetailService userDetailService;

  private final TeamMemberService teamMemberService;

  @Autowired
  public TeamManagementHandlerInterceptor(TeamService teamService, UserDetailService userDetailService,
                                          TeamMemberService teamMemberService) {
    this.teamService = teamService;
    this.userDetailService = userDetailService;
    this.teamMemberService = teamMemberService;
  }

  @Override
  public boolean preHandle(@NonNull HttpServletRequest request,
                           @NonNull HttpServletResponse response,
                           @NonNull Object handler) {

    if (handler instanceof HandlerMethod handlerMethod && hasAnnotation(handlerMethod, IsMemberOfTeamOrRegulator.class)) {
      var teamId = extractTeamIdFromRequest(request, handlerMethod);
      var user = userDetailService.getUserDetail();
      checkIsMemberOfTeam(teamId, user);
    }
    return true;
  }

  public static TeamId extractTeamIdFromRequest(HttpServletRequest httpServletRequest, HandlerMethod handlerMethod) {

    var teamIdParameter = getPathVariableByClass(handlerMethod, TeamId.class);

    if (teamIdParameter.isEmpty()) {
      var errorMessage = "No path variable called teamId found in request";
      LOGGER.warn(errorMessage);
      throw new ResponseStatusException(HttpStatus.BAD_REQUEST, errorMessage);
    }

    var pathVariables = (Map<String, String>) httpServletRequest
        .getAttribute(HandlerMapping.URI_TEMPLATE_VARIABLES_ATTRIBUTE);

    return TeamId.valueOf(pathVariables.get(teamIdParameter.get().getName()));
  }

  private void checkIsMemberOfTeam(TeamId teamId, ServiceUserDetail user) {
    var teams = teamService.getTeamsThatUserBelongsTo(user);
    var regulatorTeam = teams.stream()
        .filter(team -> team.getTeamType().equals(TeamType.REGULATOR))
        .findFirst();
    if (regulatorTeam.isPresent()) {
      return;
    }

    if (!teamMemberService.isMemberOfTeam(teamId, user)) {
      var errorMessage = "User with ID %s is not a member of team with ID %s".formatted(user.wuaId(), teamId.uuid());
      LOGGER.warn(errorMessage);
      throw new ResponseStatusException(HttpStatus.FORBIDDEN, errorMessage);
    }
  }
}