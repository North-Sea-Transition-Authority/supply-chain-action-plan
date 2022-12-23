package uk.co.nstauthority.scap.permissionmanagement.teams;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.authentication.ServiceUserDetail;
import uk.co.nstauthority.scap.energyportal.WebUserAccountId;
import uk.co.nstauthority.scap.error.exception.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.permissionmanagement.Team;
import uk.co.nstauthority.scap.permissionmanagement.TeamId;
import uk.co.nstauthority.scap.permissionmanagement.TeamMember;
import uk.co.nstauthority.scap.permissionmanagement.TeamRole;
import uk.co.nstauthority.scap.permissionmanagement.industry.IndustryTeamRole;
import uk.co.nstauthority.scap.permissionmanagement.regulator.RegulatorTeamRole;

@Service
public class TeamMemberService {

  private final TeamMemberRoleRepository teamMemberRoleRepository;

  @Autowired
  public TeamMemberService(TeamMemberRoleRepository teamMemberRoleRepository) {
    this.teamMemberRoleRepository = teamMemberRoleRepository;
  }

  public List<TeamMember> getTeamMembers(Team team) {

    // Group all roles based on the web user account id
    Map<WebUserAccountId, List<TeamMemberRole>> wuaIdToRolesMap = teamMemberRoleRepository.findAllByTeam(team)
        .stream()
        .collect(Collectors.groupingBy(teamMemberRole -> new WebUserAccountId(teamMemberRole.getWuaId())));

    // Convert to a list of TeamMember objects.
    var teamView = createTeamView(team);
    return wuaIdToRolesMap.entrySet()
        .stream()
        .map(entry -> new TeamMember(entry.getKey(), teamView,
            mapMemberRolesToTeamRoles(entry.getValue())))
        .toList();
  }

  public Optional<TeamMember> getTeamMember(Team team, WebUserAccountId wuaId) {
    // Group all roles to the appropriate wuaId
    var teamMemberRoles = teamMemberRoleRepository.findAllByTeamAndWuaId(team, wuaId.id());

    if (teamMemberRoles.isEmpty()) {
      return Optional.empty();
    }

    var teamMember = new TeamMember(wuaId, createTeamView(team), mapMemberRolesToTeamRoles(teamMemberRoles));
    return Optional.of(teamMember);
  }

  public TeamMember getTeamMemberOrThrow(Team team, WebUserAccountId wuaId) {
    return getTeamMember(team, wuaId)
        .orElseThrow(() -> new ScapEntityNotFoundException(
            "Could not find User with ID: %s in Team: %s"
                .formatted(wuaId.id(), team.getUuid())));
  }

  private TeamView createTeamView(Team team) {
    return new TeamView(new TeamId(team.getUuid()), team.getTeamType(), team.getDisplayName());
  }

  public boolean isMemberOfTeam(TeamId teamId, ServiceUserDetail user) {
    return teamMemberRoleRepository.existsByWuaIdAndTeamUuid(user.wuaId(), teamId.uuid());
  }

  public boolean isMemberOfTeamWithAnyRoleOf(TeamId teamId, ServiceUserDetail user, Set<String> roles) {
    return teamMemberRoleRepository.existsByWuaIdAndTeamUuidAndRoleIn(user.wuaId(), teamId.uuid(), roles);
  }

  public List<RolePermission> getAllPermissionsForUser(ServiceUserDetail user) {
    return mapMemberRolesToTeamRoles(teamMemberRoleRepository.findAllByWuaId(user.wuaId()))
        .stream()
        .flatMap(roles -> roles.getRolePermissions().stream())
        .toList();
  }

  private Set<TeamRole> mapMemberRolesToTeamRoles(Collection<TeamMemberRole> roles) {
    return roles.stream()
        .map(teamMemberRole -> switch (teamMemberRole.getTeam().getTeamType()) {
          case REGULATOR -> RegulatorTeamRole.valueOf(teamMemberRole.getRole());
          case INDUSTRY -> IndustryTeamRole.valueOf(teamMemberRole.getRole());
        })
        .collect(Collectors.toSet());
  }

}
