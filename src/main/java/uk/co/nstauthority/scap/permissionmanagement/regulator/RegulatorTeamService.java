package uk.co.nstauthority.scap.permissionmanagement.regulator;

import java.util.Set;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.authentication.ServiceUserDetail;
import uk.co.nstauthority.scap.error.exception.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.permissionmanagement.Team;
import uk.co.nstauthority.scap.permissionmanagement.TeamId;
import uk.co.nstauthority.scap.permissionmanagement.TeamType;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberService;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;

@Service
class RegulatorTeamService {
  private final TeamMemberService teamMemberService;

  private final TeamService teamService;

  @Autowired
  RegulatorTeamService(TeamMemberService teamMemberService,
                       TeamService teamService) {
    this.teamMemberService = teamMemberService;
    this.teamService = teamService;
  }

  Team getTeam(ServiceUserDetail user) {
    return teamService.getTeamsOfTypeThatUserBelongsTo(user, TeamType.REGULATOR)
        .stream()
        .findFirst()
        .orElseThrow(() -> new ScapEntityNotFoundException("User is not a member of regulator team"));
  }

  boolean isAccessManager(TeamId teamId, ServiceUserDetail user) {
    return teamMemberService.isMemberOfTeamWithAnyRoleOf(teamId, user, Set.of(RegulatorTeamRole.ACCESS_MANAGER.name()));
  }
}
