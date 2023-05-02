package uk.co.nstauthority.scap.endpointvalidation.rules;

import java.lang.annotation.Annotation;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;
import uk.co.nstauthority.scap.authentication.ServiceUserDetail;
import uk.co.nstauthority.scap.endpointvalidation.ScapSecurityRule;
import uk.co.nstauthority.scap.endpointvalidation.SecurityRuleResult;
import uk.co.nstauthority.scap.endpointvalidation.annotations.HasAnyPermissionForTeam;
import uk.co.nstauthority.scap.permissionmanagement.Team;
import uk.co.nstauthority.scap.permissionmanagement.TeamMember;
import uk.co.nstauthority.scap.permissionmanagement.TeamRole;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberService;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;

@Component
public class AnyPermissionForTeamRule implements ScapSecurityRule {

  private static final Logger LOGGER = LoggerFactory.getLogger(AnyPermissionForTeamRule.class);

  private final TeamMemberService teamMemberService;

  private final TeamService teamService;

  @Autowired
  public AnyPermissionForTeamRule(TeamMemberService teamMemberService,
                                  TeamService teamService) {
    this.teamMemberService = teamMemberService;
    this.teamService = teamService;
  }

  @Override
  public Class<? extends Annotation> supports() {
    return HasAnyPermissionForTeam.class;
  }

  @Override
  public SecurityRuleResult check(Object annotation, HttpServletRequest request, HttpServletResponse response,
                                  ServiceUserDetail userDetail, ScapDetail scapDetail, Team team) {
    if (team == null) {
      LOGGER.warn("Could not find Team based on URL: %s".formatted(request.getRequestURI()));
      return SecurityRuleResult.checkFailedWithStatus(HttpStatus.BAD_REQUEST);
    }

    var requiredPermissions = List.of(((HasAnyPermissionForTeam) annotation).permissions());
    var teamMemberRoles = teamMemberService.findTeamMember(team, userDetail.getWebUserAccountId())
        .map(TeamMember::roles)
        .orElse(Collections.emptySet());


    if (((HasAnyPermissionForTeam) annotation).allowRegulatorAccess()) {
      var regulatorTeamMemberRoles = teamMemberService.findTeamMember(
              teamService.getRegulatorTeam(),
              userDetail.getWebUserAccountId())
          .map(TeamMember::roles)
          .orElse(Collections.emptySet());
      teamMemberRoles = Stream.concat(teamMemberRoles.stream(), regulatorTeamMemberRoles.stream())
          .collect(Collectors.toSet());
    }

    var hasPermission = teamMemberRoles
        .stream()
        .map(TeamRole::getRolePermissions)
        .flatMap(Collection::stream)
        .anyMatch(requiredPermissions::contains);

    if (hasPermission) {
      return SecurityRuleResult.continueAsNormal();
    } else {
      return SecurityRuleResult.checkFailedWithStatus(HttpStatus.FORBIDDEN);
    }
  }
}
