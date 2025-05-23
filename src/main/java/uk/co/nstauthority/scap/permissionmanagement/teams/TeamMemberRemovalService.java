package uk.co.nstauthority.scap.permissionmanagement.teams;

import java.util.Collection;
import java.util.List;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.permissionmanagement.Team;
import uk.co.nstauthority.scap.permissionmanagement.TeamMember;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberView;
import uk.co.nstauthority.scap.permissionmanagement.TeamRole;

@Service
public class TeamMemberRemovalService extends TeamMemberPersistenceService {

  public static final String LAST_ACCESS_MANAGER_ERROR_MESSAGE = "Cannot remove the last access manager of a team";

  private final TeamMemberService teamMemberService;

  @Autowired
  TeamMemberRemovalService(TeamMemberService teamMemberService, TeamMemberRoleRepository teamMemberRoleRepository) {
    super(teamMemberRoleRepository);
    this.teamMemberService = teamMemberService;
  }

  public void removeTeamMember(Team team, TeamMember teamMember) {
    if (canRemoveTeamMember(team, teamMember)) {
      super.removeMemberFromTeam(team, teamMember);
    } else {
      throw new IllegalStateException(
          "User [%s] cannot be removed from team [%s] as they are the last access manager".formatted(
              teamMember.wuaId(), team.getUuid()));
    }
  }

  public String getRemoveScreenPageTitle(String teamName, TeamMemberView teamMemberView, boolean canRemoveTeamMember) {
    if (canRemoveTeamMember) {
      return getAskToRemovePageTitleText(teamName, teamMemberView);
    }
    return getUnableToRemovePageTitleText(teamName, teamMemberView);
  }

  String getAskToRemovePageTitleText(String teamName, TeamMemberView teamMemberView) {
    return "Are you sure you want to remove %s from %s?".formatted(teamMemberView.getDisplayName(), teamName);
  }

  String getUnableToRemovePageTitleText(String teamName, TeamMemberView teamMemberView) {
    return "Unable to remove %s from %s".formatted(teamMemberView.getDisplayName(), teamName);
  }

  public boolean canRemoveTeamMember(Team team, TeamMember teamMember) {
    var teamMembers = teamMemberService.getTeamMembers(team);
    var accessManagers = filterAccessManagers(teamMembers);
    return accessManagers.stream().anyMatch(
        tm -> !tm.wuaId().equals(teamMember.wuaId()));
  }

  private List<TeamMember> filterAccessManagers(Collection<TeamMember> teamMembers) {
    return teamMembers.stream()
        .filter(teamMember ->
            teamMember
                .roles()
                .stream()
                .map(TeamRole::getRolePermissions)
                .anyMatch(rolePermissions -> rolePermissions.contains(RolePermission.GRANT_ROLES)
                    || rolePermissions.contains(RolePermission.MANAGE_ORGANISATIONS)))
        .toList();
  }
}
