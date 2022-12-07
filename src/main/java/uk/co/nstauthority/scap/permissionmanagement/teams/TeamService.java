package uk.co.nstauthority.scap.permissionmanagement.teams;

import java.util.List;
import java.util.Optional;
import java.util.Set;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import org.springframework.validation.BindingResult;
import uk.co.nstauthority.scap.authentication.ServiceUserDetail;
import uk.co.nstauthority.scap.permissionmanagement.Team;
import uk.co.nstauthority.scap.permissionmanagement.TeamId;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberRoleService;
import uk.co.nstauthority.scap.permissionmanagement.TeamRepository;
import uk.co.nstauthority.scap.permissionmanagement.TeamType;

@Service
public class TeamService {

  private final TeamRepository teamRepository;

  private final TeamMemberRoleService teamMemberRoleService;


  @Autowired
  TeamService(TeamRepository teamRepository, TeamMemberRoleService teamMemberRoleService) {
    this.teamRepository = teamRepository;
    this.teamMemberRoleService = teamMemberRoleService;
  }

  public Optional<Team> getTeam(TeamId teamId, TeamType teamType) {
    return teamRepository.findByUuidAndTeamType(teamId.uuid(), teamType);
  }

  public Optional<Team> findByEnergyPortalOrgGroupId(int epGroupId) {
    return teamRepository.findByEnergyPortalOrgGroupId(epGroupId);
  }

  public Team createTeam(String groupName, int energyPortalGroupId, long wuaId) {
    var team = new Team();
    team.setTeamType(TeamType.INDUSTRY);
    team.setDisplayName(groupName);
    team.setEnergyPortalOrgGroupId(energyPortalGroupId);
    team = teamRepository.save(team);

    teamMemberRoleService.updateUserTeamRoles(team, wuaId, Set.of("ACCESS_MANAGER"));
    return team;
  }

  public List<Team> getTeamsOfTypeThatUserBelongsTo(ServiceUserDetail user, TeamType teamType) {
    return teamRepository.findAllTeamsOfTypeThatUserIsMemberOf(user.wuaId(), teamType);
  }

  public List<Team> getTeamsThatUserBelongsTo(ServiceUserDetail user) {
    return teamRepository.findAllTeamsThatUserIsMemberOf(user.wuaId());
  }
}
