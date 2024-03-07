package uk.co.nstauthority.scap.endpointvalidation.rules;

import java.lang.annotation.Annotation;
import java.util.List;
import java.util.stream.Collectors;
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
import uk.co.nstauthority.scap.endpointvalidation.annotations.CanAccessScap;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.permissionmanagement.Team;
import uk.co.nstauthority.scap.permissionmanagement.industry.IndustryTeamRole;
import uk.co.nstauthority.scap.permissionmanagement.regulator.RegulatorTeamRole;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberService;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;

@Component
public class CanAccessScapRule implements ScapSecurityRule {

  private static final Logger LOGGER = LoggerFactory.getLogger(CanAccessScapRule.class);
  private final TeamMemberService teamMemberService;
  private final TeamService teamService;

  @Autowired
  public CanAccessScapRule(TeamMemberService teamMemberService, TeamService teamService) {
    this.teamMemberService = teamMemberService;
    this.teamService = teamService;
  }

  @Override
  public Class<? extends Annotation> supports() {
    return CanAccessScap.class;
  }

  @Override
  public SecurityRuleResult check(Object annotation, HttpServletRequest request, HttpServletResponse response,
                                  ServiceUserDetail userDetail, ScapDetail scapDetail, Team team) {
    if (scapDetail == null) {
      LOGGER.error("Could not find SCAP based on URL: {}", request.getRequestURI());
      return SecurityRuleResult.checkFailedWithStatus(HttpStatus.BAD_REQUEST);
    }

    var industryTeam = teamService.getByEnergyPortalOrgGroupId(scapDetail.getScap().getOrganisationGroupId());
    var regulatorTeam = teamService.getRegulatorTeam();

    var teamMemberRoles =
        teamMemberService.findAllRolesByUser(List.of(industryTeam, regulatorTeam), userDetail.getWebUserAccountId())
            .stream()
            .map(teamMemberRole -> switch (teamMemberRole.getTeam().getTeamType()) {
              case REGULATOR -> RegulatorTeamRole.valueOf(teamMemberRole.getRole());
              case INDUSTRY -> IndustryTeamRole.valueOf(teamMemberRole.getRole());
            })
            .collect(Collectors.toSet());

    var permissionsForTeam = teamMemberRoles.stream()
        .flatMap(role -> role.getRolePermissions().stream())
        .collect(Collectors.toSet());

    var canViewPostSubmission = ScapDetailStatus.getPostSubmissionStatuses().contains(scapDetail.getStatus())
        && (permissionsForTeam.contains(RolePermission.SUBMIT_SCAP)
        || permissionsForTeam.contains(RolePermission.VIEW_SCAP)
        || permissionsForTeam.contains(RolePermission.REVIEW_SCAP));

    var canAccessDraftScap = ScapDetailStatus.DRAFT.equals(scapDetail.getStatus())
        && permissionsForTeam.contains(RolePermission.SUBMIT_SCAP);

    if (canAccessDraftScap || canViewPostSubmission) {
      return SecurityRuleResult.continueAsNormal();
    }
    return SecurityRuleResult.checkFailedWithStatus(HttpStatus.FORBIDDEN);
  }
}
