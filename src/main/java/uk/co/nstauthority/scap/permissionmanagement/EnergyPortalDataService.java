package uk.co.nstauthority.scap.permissionmanagement;


import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.toSet;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;
import org.springframework.stereotype.Service;
import uk.co.fivium.energyportal.serviceproviders.epmq.ScopeType;
import uk.co.fivium.energyportal.serviceproviders.epmq.messages.ServiceProviderTeamDto;
import uk.co.fivium.energyportal.serviceproviders.epmq.messages.ServiceProviderTeamTypeRoleDto;
import uk.co.fivium.energyportal.serviceproviders.epmq.messages.ServiceProviderUserTeamRolesDto;
import uk.co.fivium.energyportal.starter.serviceproviders.EnergyPortalServiceProviderDataService;
import uk.co.nstauthority.scap.permissionmanagement.industry.IndustryTeamRole;
import uk.co.nstauthority.scap.permissionmanagement.regulator.RegulatorTeamRole;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberRole;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberRoleRepository;

@Service
class EnergyPortalDataService implements EnergyPortalServiceProviderDataService {

  private final TeamRepository teamRepository;
  private final TeamMemberRoleRepository teamRoleRepository;

  EnergyPortalDataService(
      TeamRepository teamRepository,
      TeamMemberRoleRepository teamRoleRepository
  ) {
    this.teamRepository = teamRepository;
    this.teamRoleRepository = teamRoleRepository;
  }

  @Override
  public Collection<ServiceProviderTeamDto> getServiceProviderTeamDtos() {
    return teamRepository.findAll()
        .stream()
        .map(team -> new ServiceProviderTeamDto(
            team.getUuid().toString(),
            Objects.toString(team.getEnergyPortalOrgGroupId(), null),
            ScopeType.ORGANISATION_GROUP,
            team.getTeamType().name()
        ))
        .collect(toSet());
  }

  @Override
  public Map<String, Collection<ServiceProviderTeamTypeRoleDto>> getTeamTypeToServiceProviderTeamTypeRoleDtos() {
    Map<String, Collection<ServiceProviderTeamTypeRoleDto>> teamTypeToRoles = new HashMap<>();

    teamTypeToRoles.put(
        TeamType.INDUSTRY.name(),
        Arrays.stream(IndustryTeamRole.values())
            .map(teamRole -> new ServiceProviderTeamTypeRoleDto(
                teamRole.name(),
                teamRole.getDisplayName(),
                teamRole.getDescription(),
                teamRole == IndustryTeamRole.ACCESS_MANAGER,
                teamRole.getDisplayOrder()
            )).collect(Collectors.toSet())
    );

    teamTypeToRoles.put(
        TeamType.REGULATOR.name(),
        Arrays.stream(RegulatorTeamRole.values())
            .map(teamRole -> new ServiceProviderTeamTypeRoleDto(
                teamRole.name(),
                teamRole.getDisplayName(),
                teamRole.getDescription(),
                teamRole == RegulatorTeamRole.ACCESS_MANAGER,
                teamRole.getDisplayOrder()
            )).collect(Collectors.toSet())
    );

    return teamTypeToRoles;
  }

  @Override
  public Collection<String> getTeamTypes() {
    return Arrays.stream(TeamType.values())
        .map(TeamType::name)
        .collect(toSet());
  }

  @Override
  public Collection<ServiceProviderUserTeamRolesDto> getServiceProviderUserTeamRolesDtos() {
    var serviceProviderUserTeamRolesDtos = new HashSet<ServiceProviderUserTeamRolesDto>();

    var wuaIdToTeamRoles = teamRoleRepository.findAll()
        .stream()
        .collect(groupingBy(TeamMemberRole::getWuaId, toSet()));

    for (var wuaIdToTeamRoleEntry : wuaIdToTeamRoles.entrySet()) {
      var teamToTeamRoles = wuaIdToTeamRoleEntry.getValue()
          .stream()
          .collect(groupingBy(TeamMemberRole::getTeam, toSet()));

      for (var teamToTeamRoleEntry : teamToTeamRoles.entrySet()) {
        var wuaId = wuaIdToTeamRoleEntry.getKey();
        var team = teamToTeamRoleEntry.getKey();
        var roles = teamToTeamRoleEntry.getValue()
            .stream()
            .map(TeamMemberRole::getRole)
            .collect(toSet());

        serviceProviderUserTeamRolesDtos.add(new ServiceProviderUserTeamRolesDto(
            wuaId,
            String.valueOf(team.getUuid()),
            team.getTeamType().name(),
            roles
        ));
      }
    }
    return serviceProviderUserTeamRolesDtos;
  }
}