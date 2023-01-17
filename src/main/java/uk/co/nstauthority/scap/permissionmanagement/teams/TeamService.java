package uk.co.nstauthority.scap.permissionmanagement.teams;

import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import org.springframework.validation.BindingResult;
import uk.co.nstauthority.scap.authentication.ServiceUserDetail;
import uk.co.nstauthority.scap.energyportal.EnergyPortalUserDto;
import uk.co.nstauthority.scap.error.exception.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.permissionmanagement.Team;
import uk.co.nstauthority.scap.permissionmanagement.TeamId;
import uk.co.nstauthority.scap.permissionmanagement.TeamRepository;
import uk.co.nstauthority.scap.permissionmanagement.TeamRole;
import uk.co.nstauthority.scap.permissionmanagement.TeamType;

@Service
public class TeamService {

  private final TeamRepository teamRepository;

  private final TeamMemberRoleService teamMemberRoleService;

  private final NewTeamFormvalidator newTeamFormvalidator;

  @Autowired
  protected TeamService(TeamRepository teamRepository, TeamMemberRoleService teamMemberRoleService,
                        @Lazy NewTeamFormvalidator newTeamFormvalidator) {
    this.teamRepository = teamRepository;
    this.teamMemberRoleService = teamMemberRoleService;
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

  public Optional<Team> findByEnergyPortalOrgGroupId(int epGroupId) {
    return teamRepository.findByEnergyPortalOrgGroupId(epGroupId);
  }

  public Team getByEnergyPortalOrgGroupId(int epGroupId) {
    return teamRepository.findByEnergyPortalOrgGroupId(epGroupId)
        .orElseThrow(() -> new ScapEntityNotFoundException(
            "Could not find Team associated with energy portal organisation group ID: %s".formatted(epGroupId)));
  }

  public Team createTeam(String groupName, int energyPortalGroupId) {
    var team = new Team();
    team.setTeamType(TeamType.INDUSTRY);
    team.setDisplayName(groupName);
    team.setEnergyPortalOrgGroupId(energyPortalGroupId);
    team = teamRepository.save(team);
    return team;
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
        .anyMatch(team -> team.getEnergyPortalOrgGroupId().equals(organisationGroupId));
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
}
