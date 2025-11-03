package uk.co.nstauthority.scap.permissionmanagement.teams;

import java.util.ArrayList;
import java.util.Set;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import uk.co.fivium.energyportal.starter.serviceproviders.EnergyPortalServiceProviderUserRolesService;
import uk.co.nstauthority.scap.authentication.UserDetailService;
import uk.co.nstauthority.scap.energyportal.EnergyPortalUserDto;
import uk.co.nstauthority.scap.permissionmanagement.Team;

@Service
public class TeamMemberRoleService {

  private final TeamMemberRoleRepository teamMemberRoleRepository;

  private final EnergyPortalServiceProviderUserRolesService energyPortalServiceProviderUserRolesService;

  @Autowired
  public TeamMemberRoleService(TeamMemberRoleRepository teamMemberRoleRepository,
                               UserDetailService userDetailService,
                               EnergyPortalServiceProviderUserRolesService energyPortalServiceProviderUserRolesService) {
    this.teamMemberRoleRepository = teamMemberRoleRepository;
    this.energyPortalServiceProviderUserRolesService = energyPortalServiceProviderUserRolesService;
  }

  @Transactional
  public void addUserTeamRoles(Team team, EnergyPortalUserDto userToAdd, Set<String> roles) {
    updateUserTeamRoles(team, userToAdd.webUserAccountId(), roles);
  }

  @Transactional
  public void addUserTeamRoles(Team team, long userToAdd, Set<String> roles) {
    updateUserTeamRoles(team, userToAdd, roles);
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
    teamMemberRoleRepository.findAllByTeam(team).stream().map(TeamMemberRole::getWuaId).forEach(
        wuaId -> energyPortalServiceProviderUserRolesService.publishRemoveUserFromTeam(
            wuaId,
            String.valueOf(team.getUuid())
        )
    );
    teamMemberRoleRepository.deleteAllByTeam(team);
  }
}
