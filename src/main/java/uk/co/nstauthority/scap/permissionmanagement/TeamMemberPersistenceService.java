package uk.co.nstauthority.scap.permissionmanagement;

import org.springframework.beans.factory.annotation.Autowired;

public abstract class TeamMemberPersistenceService {

  @Autowired
  private TeamMemberRoleRepository teamMemberRoleRepository;

  protected void removeMemberFromTeam(Team team, TeamMember teamMember) {
    var teamMemberRoles = teamMemberRoleRepository.findAllByTeamAndWuaId(team, teamMember.wuaId().id());
    teamMemberRoleRepository.deleteAll(teamMemberRoles);
  }

}
