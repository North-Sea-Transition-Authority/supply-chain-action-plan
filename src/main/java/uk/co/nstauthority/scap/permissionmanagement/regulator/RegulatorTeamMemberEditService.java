package uk.co.nstauthority.scap.permissionmanagement.regulator;

import java.util.Set;
import javax.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.permissionmanagement.Team;
import uk.co.nstauthority.scap.permissionmanagement.TeamMember;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberRoleService;

@Service
class RegulatorTeamMemberEditService {

  private final TeamMemberRoleService teamMemberRoleService;

  @Autowired
  RegulatorTeamMemberEditService(
      TeamMemberRoleService teamMemberRoleService) {
    this.teamMemberRoleService = teamMemberRoleService;
  }

  @Transactional
  void updateRoles(Team team, TeamMember teamMember, Set<String> newRoles) {
    teamMemberRoleService.updateUserTeamRoles(team, teamMember.wuaId().id(), newRoles);
  }
}
