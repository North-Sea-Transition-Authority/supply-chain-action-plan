package uk.co.nstauthority.scap.permissionmanagement.industry;

import java.util.Optional;
import java.util.Set;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.authentication.ServiceUserDetail;
import uk.co.nstauthority.scap.error.exception.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.permissionmanagement.Team;
import uk.co.nstauthority.scap.permissionmanagement.TeamId;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberService;
import uk.co.nstauthority.scap.permissionmanagement.TeamType;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;

@Service
class IndustryTeamService {

  private final TeamService teamService;

  private final TeamMemberService teamMemberService;

  @Autowired
  IndustryTeamService(TeamService teamService, TeamMemberService teamMemberService) {
    this.teamService = teamService;
    this.teamMemberService = teamMemberService;
  }

  Optional<Team> getTeam(TeamId teamId) {
    return teamService.getTeam(teamId, TeamType.INDUSTRY);
  }

  Team getTeamOrThrow(TeamId teamId) {
    return teamService.getTeam(teamId, TeamType.INDUSTRY)
        .orElseThrow(() -> new ScapEntityNotFoundException(
            "Could not find Team with ID: %s".formatted(teamId)));
  }

  boolean isAccessManager(TeamId teamId, ServiceUserDetail user) {
    return teamMemberService.isMemberOfTeamWithAnyRoleOf(teamId, user, Set.of(IndustryTeamRole.ACCESS_MANAGER.name()));
  }
}
