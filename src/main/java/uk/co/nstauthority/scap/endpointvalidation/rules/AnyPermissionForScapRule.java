package uk.co.nstauthority.scap.endpointvalidation.rules;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.lang.annotation.Annotation;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;
import uk.co.nstauthority.scap.authentication.ServiceUserDetail;
import uk.co.nstauthority.scap.endpointvalidation.ScapSecurityRule;
import uk.co.nstauthority.scap.endpointvalidation.SecurityRuleResult;
import uk.co.nstauthority.scap.endpointvalidation.annotations.HasAnyPermissionForScap;
import uk.co.nstauthority.scap.permissionmanagement.Team;
import uk.co.nstauthority.scap.permissionmanagement.TeamMember;
import uk.co.nstauthority.scap.permissionmanagement.TeamRole;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberService;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;

@Component
public class AnyPermissionForScapRule implements ScapSecurityRule {

  private static final Logger LOGGER = LoggerFactory.getLogger(AnyPermissionForScapRule.class);

  private final TeamService teamService;

  private final TeamMemberService teamMemberService;

  public AnyPermissionForScapRule(TeamService teamService, TeamMemberService teamMemberService) {
    this.teamService = teamService;
    this.teamMemberService = teamMemberService;
  }

  @Override
  public Class<? extends Annotation> supports() {
    return HasAnyPermissionForScap.class;
  }

  @Override
  public SecurityRuleResult check(Object annotation,
                                  HttpServletRequest request,
                                  HttpServletResponse response,
                                  ServiceUserDetail userDetail,
                                  ScapDetail scapDetail,
                                  Team team) {
    if (scapDetail == null) {
      LOGGER.error("Could not find SCAP based on URL: {}", request.getRequestURI());
      return SecurityRuleResult.checkFailedWithStatus(HttpStatus.BAD_REQUEST);
    }

    var requiredPermissions = List.of(((HasAnyPermissionForScap) annotation).permissions());
    team = teamService.getByEnergyPortalOrgGroupId(scapDetail.getScap().getOrganisationGroupId());
    var teamMemberRoles = teamMemberService.findTeamMember(team, userDetail.getWebUserAccountId())
        .map(TeamMember::roles)
        .orElse(Collections.emptySet());


    if (((HasAnyPermissionForScap) annotation).allowRegulatorAccess()) {
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
      LOGGER.error(
          "User does not have permission to interact with SCAP: {} associated with Org Group: {}",
          scapDetail.getScap().getReference(),
          scapDetail.getScap().getOrganisationGroupId()
      );
      return SecurityRuleResult.checkFailedWithStatus(HttpStatus.FORBIDDEN);
    }
  }
}
