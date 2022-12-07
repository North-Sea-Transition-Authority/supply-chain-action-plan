package uk.co.nstauthority.scap.permissionmanagement.regulator;

import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.authentication.ServiceUserDetail;
import uk.co.nstauthority.scap.energyportal.EnergyPortalUserDto;
import uk.co.nstauthority.scap.error.exception.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.permissionmanagement.Team;
import uk.co.nstauthority.scap.permissionmanagement.TeamId;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberRoleService;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberService;
import uk.co.nstauthority.scap.permissionmanagement.TeamType;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;

@Service
class RegulatorTeamService {

  private final TeamService teamService;

  private final TeamMemberService teamMemberService;

  private final TeamMemberRoleService teamMemberRoleService;

  @Autowired
  RegulatorTeamService(TeamService teamService, TeamMemberService teamMemberService,
                       TeamMemberRoleService teamMemberRoleService) {
    this.teamService = teamService;
    this.teamMemberService = teamMemberService;
    this.teamMemberRoleService = teamMemberRoleService;
  }

  Optional<Team> getRegulatorTeamForUser(ServiceUserDetail user) {
    return teamService.getTeamsOfTypeThatUserBelongsTo(user, TeamType.REGULATOR)
        .stream()
        .findFirst();
  }

  Team getRegulatorTeamForUserOrThrow(ServiceUserDetail user) {
    return getRegulatorTeamForUser(user)
        .orElseThrow(() -> new ScapEntityNotFoundException("User is not a member of regulator team"));
  }

  Optional<Team> getTeam(TeamId teamId) {
    return teamService.getTeam(teamId, TeamType.REGULATOR);
  }

  Team getTeamOrThrow(TeamId teamId) {
    return getTeam(teamId)
        .orElseThrow(() -> new ScapEntityNotFoundException("User is not a member of regulator team"));
  }

  boolean isAccessManager(TeamId teamId, ServiceUserDetail user) {
    return teamMemberService.isMemberOfTeamWithAnyRoleOf(teamId, user, Set.of(RegulatorTeamRole.ACCESS_MANAGER.name()));
  }

  void addUserTeamRoles(Team team, EnergyPortalUserDto userToAdd, Set<RegulatorTeamRole> roles) {
    var rolesAsStrings = roles
        .stream()
        .map(RegulatorTeamRole::name)
        .collect(Collectors.toSet());

    teamMemberRoleService.addUserTeamRoles(team, userToAdd, rolesAsStrings);
  }
}
