package uk.co.nstauthority.scap.permissionmanagement.teams;

import static uk.co.nstauthority.scap.permissionmanagement.TeamType.REGULATOR;

import jakarta.transaction.Transactional;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import org.springframework.validation.BindingResult;
import uk.co.nstauthority.scap.authentication.ServiceUserDetail;
import uk.co.nstauthority.scap.energyportal.EnergyPortalUserDto;
import uk.co.nstauthority.scap.energyportal.WebUserAccountId;
import uk.co.nstauthority.scap.error.exception.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.permissionmanagement.Team;
import uk.co.nstauthority.scap.permissionmanagement.TeamId;
import uk.co.nstauthority.scap.permissionmanagement.TeamRepository;
import uk.co.nstauthority.scap.permissionmanagement.TeamRole;
import uk.co.nstauthority.scap.permissionmanagement.TeamType;
import uk.co.nstauthority.scap.permissionmanagement.industry.IndustryTeamRole;
import uk.co.nstauthority.scap.permissionmanagement.regulator.RegulatorTeamRole;

@Service
public class TeamService {

  private final TeamRepository teamRepository;

  private final TeamMemberRoleService teamMemberRoleService;

  private final TeamMemberService teamMemberService;

  private final NewTeamFormValidator newTeamFormvalidator;

  @Autowired
  protected TeamService(TeamRepository teamRepository,
                        TeamMemberRoleService teamMemberRoleService,
                        TeamMemberService teamMemberService,
                        @Lazy NewTeamFormValidator newTeamFormvalidator) {
    this.teamRepository = teamRepository;
    this.teamMemberRoleService = teamMemberRoleService;
    this.teamMemberService = teamMemberService;
    this.newTeamFormvalidator = newTeamFormvalidator;
  }

  public Iterable<Team> getAllTeams() {
    return teamRepository.findAll();
  }

  public Optional<Team> findTeam(TeamId teamId) {
    return teamRepository.findByUuid(teamId.uuid());
  }

  public Team getTeam(TeamId teamId) {
    return findTeam(teamId)
        .orElseThrow(() -> new ScapEntityNotFoundException(
            "No team with ID %s found".formatted(teamId.uuid())
        ));
  }

  public List<TeamView> findTeamsByUser(ServiceUserDetail user) {
    List<Team> teams;
    var isOrgAccessManager = teamMemberService.isMemberOfTeamWithAnyRoleOf(
        new TeamId(getRegulatorTeam().getUuid()),
        user,
        Collections.singleton(RegulatorTeamRole.ORGANISATION_ACCESS_MANAGER.name()));

    if (isOrgAccessManager) {
      teams = StreamSupport.stream(getAllTeams().spliterator(), false)
          .toList();
    } else {
      teams = getTeamsThatUserBelongsTo(user);
    }
    return teams
        .stream()
        .map(TeamView::fromTeam)
        .toList();
  }

  public Optional<Team> findByEnergyPortalOrgGroupId(int epGroupId) {
    return teamRepository.findByEnergyPortalOrgGroupId(epGroupId);
  }

  public Team getByEnergyPortalOrgGroupId(int epGroupId) {
    return teamRepository.findByEnergyPortalOrgGroupId(epGroupId)
        .orElseThrow(() -> new ScapEntityNotFoundException(
            "Could not find Team associated with energy portal organisation group ID: %s".formatted(epGroupId)));
  }

  @Transactional
  public Team createTeam(String groupName, int energyPortalGroupId) {
    var team = new Team();
    team.setTeamType(TeamType.INDUSTRY);
    team.setDisplayName(groupName);
    team.setEnergyPortalOrgGroupId(energyPortalGroupId);
    team = teamRepository.save(team);
    return team;
  }

  public Team getRegulatorTeam() {
    return teamRepository.getTeamByTeamType(REGULATOR);
  }

  public List<Team> getTeamsOfTypeThatUserBelongsTo(ServiceUserDetail user, TeamType teamType) {
    return teamRepository.findAllTeamsOfTypeThatUserIsMemberOf(user.wuaId(), teamType);
  }

  public List<Team> getTeamsThatUserBelongsTo(ServiceUserDetail user) {
    return teamRepository.findAllTeamsThatUserIsMemberOf(user.wuaId());
  }

  public boolean userIsMemberOfOrganisationGroupTeam(Integer organisationGroupId, ServiceUserDetail user) {
    return getTeamsThatUserBelongsTo(user)
        .stream()
        .anyMatch(team -> Objects.equals(team.getEnergyPortalOrgGroupId(), organisationGroupId));
  }

  public boolean userIsMemberOfRegulatorTeam(ServiceUserDetail user) {
    return getTeamsThatUserBelongsTo(user)
        .stream()
        .anyMatch(team -> REGULATOR.equals(team.getTeamType()));
  }

  public BindingResult validate(NewTeamForm form, BindingResult bindingResult) {
    newTeamFormvalidator.validate(form, bindingResult);
    return bindingResult;
  }

  public void addUserTeamRoles(Team team, EnergyPortalUserDto userToAdd, Set<? extends TeamRole> roles) {
    var rolesAsStrings = roles
        .stream()
        .map(TeamRole::name)
        .collect(Collectors.toSet());

    teamMemberRoleService.addUserTeamRoles(team, userToAdd, rolesAsStrings);
  }

  @Transactional
  public void archiveTeam(Team team) {
    teamMemberRoleService.deleteUsersInTeam(team);
    teamRepository.delete(team);
  }

  public List<RolePermission> findAllPermissionsForUserInOrganisationGroup(Long wuaId, Integer organisationGroupId) {
    var team = teamRepository.findByEnergyPortalOrgGroupId(organisationGroupId);

    if (team.isEmpty()) {
      return List.of();
    }

    return teamMemberService.findTeamMember(team.get(), new WebUserAccountId(wuaId))
        .stream()
        .flatMap(teamMember -> teamMember.roles().stream())
        .flatMap(teamRole -> teamRole.getRolePermissions().stream())
        .toList();
  }

  public List<Team> findAllTeamsForUserBasedOnPermission(List<TeamRole> teamRoles, Long wuaId) {
    var teams = teamRepository.findAllTeamsThatUserIsMemberOf(wuaId);
    var allRolesForUser = teamMemberService.findAllRolesByUser(teams, new WebUserAccountId(wuaId));

    var teamsUserIsIn = new ArrayList<Team>();

    for (var teamMemberRole : allRolesForUser) {
      if (TeamType.INDUSTRY.equals(teamMemberRole.getTeam().getTeamType())
          && teamRoles.contains(IndustryTeamRole.valueOf(teamMemberRole.getRole()))) {
        teamsUserIsIn.add(teamMemberRole.getTeam());
      }

      if (TeamType.REGULATOR.equals(teamMemberRole.getTeam().getTeamType())
          && teamRoles.contains(RegulatorTeamRole.valueOf(teamMemberRole.getRole()))) {
        teamsUserIsIn.add(teamMemberRole.getTeam());
      }
    }

    return teamsUserIsIn;
  }
}
