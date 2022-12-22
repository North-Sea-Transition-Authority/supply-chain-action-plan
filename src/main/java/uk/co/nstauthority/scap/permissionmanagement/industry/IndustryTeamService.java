package uk.co.nstauthority.scap.permissionmanagement.industry;

import java.util.Set;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.authentication.ServiceUserDetail;
import uk.co.nstauthority.scap.permissionmanagement.TeamId;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberService;

@Service
class IndustryTeamService {

  private final TeamMemberService teamMemberService;

  @Autowired
  IndustryTeamService(TeamMemberService teamMemberService) {
    this.teamMemberService = teamMemberService;
  }

  boolean isAccessManager(TeamId teamId, ServiceUserDetail user) {
    return teamMemberService.isMemberOfTeamWithAnyRoleOf(teamId, user, Set.of(IndustryTeamRole.ACCESS_MANAGER.name()));
  }
}
