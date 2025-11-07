package uk.co.nstauthority.scap.permissionmanagement.teams;

import java.util.ArrayList;
import java.util.Set;
import java.util.stream.Collectors;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import uk.co.fivium.energyportal.starter.accounts.EnergyPortalServiceAccessService;
import uk.co.fivium.energyportal.starter.serviceproviders.EnergyPortalServiceProviderUserRolesService;
import uk.co.nstauthority.scap.permissionmanagement.Team;

@Service
public class TeamMemberRoleService {

  private final TeamMemberRoleRepository teamMemberRoleRepository;
  private final EnergyPortalServiceProviderUserRolesService energyPortalServiceProviderUserRolesService;
  private final TeamMemberService teamMemberService;
  private final EnergyPortalServiceAccessService energyPortalServiceAccessService;

  @Autowired
  public TeamMemberRoleService(
      TeamMemberRoleRepository teamMemberRoleRepository,
      EnergyPortalServiceProviderUserRolesService energyPortalServiceProviderUserRolesService,
      TeamMemberService teamMemberService,
      EnergyPortalServiceAccessService energyPortalServiceAccessService
  ) {
    this.teamMemberRoleRepository = teamMemberRoleRepository;
    this.energyPortalServiceProviderUserRolesService = energyPortalServiceProviderUserRolesService;
    this.teamMemberService = teamMemberService;
    this.energyPortalServiceAccessService = energyPortalServiceAccessService;
  }

  @Transactional
  public void updateUserTeamRoles(Team team, long wuaId, Set<String> roles) {
    // Clear user's existing roles
    teamMemberRoleRepository.deleteAllByTeamAndWuaId(team, wuaId);

    var teamMemberRoles = new ArrayList<TeamMemberRole>();

    // Create new roles based on role selection
    roles.forEach(role -> {
      var teamMemberRole = new TeamMemberRole();
      teamMemberRole.setTeam(team);
      teamMemberRole.setWuaId(wuaId);
      teamMemberRole.setRole(role);
      teamMemberRoles.add(teamMemberRole);
    });

    if (teamMemberService.getAllPermissionsForUser(wuaId).isEmpty()) {
      energyPortalServiceAccessService.addUser(wuaId);
    }

    teamMemberRoleRepository.saveAll(teamMemberRoles);

    energyPortalServiceProviderUserRolesService.publishUsersRolesForTeam(
        wuaId,
        team.getUuid().toString(),
        team.getTeamType().name(),
        roles
    );
  }

  @Transactional
  public void deleteUsersInTeam(Team team) {
    var wuaIds = teamMemberRoleRepository.findAllByTeam(team).stream().map(TeamMemberRole::getWuaId).collect(Collectors.toSet());
    wuaIds.forEach(
        wuaId -> energyPortalServiceProviderUserRolesService.publishRemoveUserFromTeam(
            wuaId,
            String.valueOf(team.getUuid())
        )
    );
    teamMemberRoleRepository.deleteAllByTeam(team);

    var teamMemberRoles = teamMemberRoleRepository.findAllByWuaIdIn(wuaIds);

    wuaIds.forEach(
        wuaId -> {
          if (teamMemberRoles.stream().noneMatch(teamMemberRole -> teamMemberRole.getWuaId().equals(wuaId))) {
            energyPortalServiceAccessService.removeUser(wuaId);
          }
        }
    );
  }
}
