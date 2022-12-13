package uk.co.nstauthority.scap.permissionmanagement.teams;

import org.springframework.beans.factory.annotation.Autowired;
import uk.co.nstauthority.scap.permissionmanagement.Team;
import uk.co.nstauthority.scap.permissionmanagement.TeamMember;

public abstract class TeamMemberPersistenceService {

  @Autowired
  private TeamMemberRoleRepository teamMemberRoleRepository;

  protected TeamMemberPersistenceService(TeamMemberRoleRepository teamMemberRoleRepository) {
    this.teamMemberRoleRepository = teamMemberRoleRepository;
  }

  protected void removeMemberFromTeam(Team team, TeamMember teamMember) {
    var teamMemberRoles = teamMemberRoleRepository.findAllByTeamAndWuaId(team, teamMember.wuaId().id());
    teamMemberRoleRepository.deleteAll(teamMemberRoles);
  }

}
