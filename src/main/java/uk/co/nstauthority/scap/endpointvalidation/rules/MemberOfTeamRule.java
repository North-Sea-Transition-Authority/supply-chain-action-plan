package uk.co.nstauthority.scap.endpointvalidation.rules;

import java.lang.annotation.Annotation;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;
import uk.co.nstauthority.scap.authentication.ServiceUserDetail;
import uk.co.nstauthority.scap.endpointvalidation.ScapSecurityRule;
import uk.co.nstauthority.scap.endpointvalidation.SecurityRuleResult;
import uk.co.nstauthority.scap.endpointvalidation.annotations.IsMemberOfTeam;
import uk.co.nstauthority.scap.permissionmanagement.Team;
import uk.co.nstauthority.scap.permissionmanagement.TeamId;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberService;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;

@Component
public class MemberOfTeamRule implements ScapSecurityRule {

  private final TeamService teamService;

  private final TeamMemberService teamMemberService;

  @Autowired
  public MemberOfTeamRule(TeamService teamService, TeamMemberService teamMemberService) {
    this.teamService = teamService;
    this.teamMemberService = teamMemberService;
  }

  @Override
  public Class<? extends Annotation> supports() {
    return IsMemberOfTeam.class;
  }

  @Override
  public SecurityRuleResult check(Object annotation,
                                  HttpServletRequest request,
                                  HttpServletResponse response,
                                  ServiceUserDetail userDetail,
                                  ScapDetail scapDetail,
                                  Team team) {

    if (((IsMemberOfTeam) annotation).allowRegulatorAccess() && teamService.userIsMemberOfRegulatorTeam(userDetail)) {
      return SecurityRuleResult.continueAsNormal();
    }
    if (team == null) {
      return SecurityRuleResult.checkFailedWithStatus(HttpStatus.BAD_REQUEST);
    }
    if (teamMemberService.isMemberOfTeam(new TeamId(team.getUuid()), userDetail)) {
      return SecurityRuleResult.continueAsNormal();
    }
    return SecurityRuleResult.checkFailedWithStatus(HttpStatus.FORBIDDEN);
  }
}
