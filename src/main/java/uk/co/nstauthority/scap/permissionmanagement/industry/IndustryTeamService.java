package uk.co.nstauthority.scap.permissionmanagement.industry;

import java.util.Set;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.authentication.ServiceUserDetail;
import uk.co.nstauthority.scap.permissionmanagement.TeamId;
import uk.co.nstauthority.scap.permissionmanagement.TeamType;
import uk.co.nstauthority.scap.permissionmanagement.regulator.RegulatorTeamRole;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberService;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;

@Service
class IndustryTeamService {

  private final TeamMemberService teamMemberService;

  private final TeamService teamService;

  @Autowired
  IndustryTeamService(TeamMemberService teamMemberService, TeamService teamService) {
    this.teamMemberService = teamMemberService;
    this.teamService = teamService;
  }

  boolean isAccessManager(TeamId teamId, ServiceUserDetail user) {
    if (teamMemberService.isMemberOfTeamWithAnyRoleOf(teamId, user, Set.of(IndustryTeamRole.ACCESS_MANAGER.name()))) {
      return true;
    }
    var regulatorTeam = teamService.getTeamsOfTypeThatUserBelongsTo(user, TeamType.REGULATOR);
    if (!(regulatorTeam.isEmpty())) {
      var regulatorTeamId = new TeamId(regulatorTeam.get(0).getUuid());
      return teamMemberService
          .isMemberOfTeamWithAnyRoleOf(regulatorTeamId, user, Set.of(RegulatorTeamRole.ORGANISATION_ACCESS_MANAGER.name()));
    }
    return false;
  }
}
