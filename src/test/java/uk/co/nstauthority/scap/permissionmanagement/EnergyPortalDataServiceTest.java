package uk.co.nstauthority.scap.permissionmanagement;


import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.when;

import java.util.List;
import java.util.Map;
import java.util.Set;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.fivium.energyportal.serviceproviders.epmq.ScopeType;
import uk.co.fivium.energyportal.serviceproviders.epmq.messages.ServiceProviderTeamDto;
import uk.co.fivium.energyportal.serviceproviders.epmq.messages.ServiceProviderTeamTypeRoleDto;
import uk.co.fivium.energyportal.serviceproviders.epmq.messages.ServiceProviderUserTeamRolesDto;
import uk.co.nstauthority.scap.permissionmanagement.industry.IndustryTeamRole;
import uk.co.nstauthority.scap.permissionmanagement.regulator.RegulatorTeamRole;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberRoleRepository;

@ExtendWith(MockitoExtension.class)
class EnergyPortalDataServiceTest {

  @Mock
  private TeamRepository teamRepository;

  @Mock
  private TeamMemberRoleRepository teamMemberRoleRepository;

  @InjectMocks
  private EnergyPortalDataService energyPortalDataService;

  @Test
  void getServiceProviderTeamDtos() {
    var team1 = TeamTestUtil.Builder().withTeamType(TeamType.INDUSTRY).build();
    var team2 = TeamTestUtil.Builder().withTeamType(TeamType.REGULATOR).build();

    var expectedDto1 = new ServiceProviderTeamDto(
        team1.getUuid().toString(),
        String.valueOf(team1.getEnergyPortalOrgGroupId()),
        ScopeType.ORGANISATION_GROUP,
        team1.getTeamType().name()
    );
    var expectedDto2 = new ServiceProviderTeamDto(
        team2.getUuid().toString(),
        null,
        ScopeType.ORGANISATION_GROUP,
        team2.getTeamType().name()
    );

    when(teamRepository.findAll()).thenReturn(List.of(team1, team2));

    assertThat(energyPortalDataService.getServiceProviderTeamDtos())
        .containsExactlyInAnyOrder(expectedDto1, expectedDto2);
  }

  @Test
  void getTeamTypeToServiceProviderTeamTypeRoleDtos() {
    var regulatorServiceRoleDtos = Set.of(
        createServiceRoleDto(RegulatorTeamRole.ACCESS_MANAGER, true),
        createServiceRoleDto(RegulatorTeamRole.ORGANISATION_ACCESS_MANAGER, false),
        createServiceRoleDto(RegulatorTeamRole.SCAP_CASE_OFFICER, false),
        createServiceRoleDto(RegulatorTeamRole.SCAP_VIEWER, false)
    );

    var organisationServiceRoleDtos = Set.of(
        createServiceRoleDto(IndustryTeamRole.ACCESS_MANAGER, true),
        createServiceRoleDto(IndustryTeamRole.SCAP_SUBMITTER, false),
        createServiceRoleDto(IndustryTeamRole.SCAP_VIEWER, false)
    );

    assertThat(energyPortalDataService.getTeamTypeToServiceProviderTeamTypeRoleDtos())
        .isEqualTo(
            Map.of(
                TeamType.REGULATOR.name(), regulatorServiceRoleDtos,
                TeamType.INDUSTRY.name(), organisationServiceRoleDtos
            )
        );
  }

  @Test
  void getTeamTypes() {
    assertThat(energyPortalDataService.getTeamTypes()).isEqualTo(Set.of(
        TeamType.REGULATOR.name(),
        TeamType.INDUSTRY.name()
    ));
  }


  @Test
  void getServiceProviderUserTeamRolesDtos() {
    var team1 = TeamTestUtil.Builder().build();
    var team2 = TeamTestUtil.Builder().build();

    var wuaId1Team1TeamRole = TeamMemberRoleTestUtil.newBuilder()
        .withWuaId(1L)
        .withTeam(team1)
        .withRole(RegulatorTeamRole.ACCESS_MANAGER.name())
        .build();

    var wuaId1Team2TeamRole = TeamMemberRoleTestUtil.newBuilder()
        .withWuaId(1L)
        .withTeam(team2)
        .withRole(RegulatorTeamRole.SCAP_VIEWER.name())
        .build();

    var wuaId2Team2TeamRole1 = TeamMemberRoleTestUtil.newBuilder()
        .withWuaId(2L)
        .withTeam(team2)
        .withRole(IndustryTeamRole.ACCESS_MANAGER.name())
        .build();

    var wuaId2Team2TeamRole2 = TeamMemberRoleTestUtil.newBuilder()
        .withWuaId(2L)
        .withTeam(team2)
        .withRole(IndustryTeamRole.SCAP_VIEWER.name())
        .build();

    when(teamMemberRoleRepository.findAll()).thenReturn(List.of(
        wuaId1Team1TeamRole,
        wuaId1Team2TeamRole,
        wuaId2Team2TeamRole1,
        wuaId2Team2TeamRole2
    ));

    assertThat(energyPortalDataService.getServiceProviderUserTeamRolesDtos())
        .containsExactlyInAnyOrder(
            new ServiceProviderUserTeamRolesDto(
                1L,
                team1.getUuid().toString(),
                team1.getTeamType().name(),
                Set.of(RegulatorTeamRole.ACCESS_MANAGER.name())
            ),
            new ServiceProviderUserTeamRolesDto(
                1L,
                team2.getUuid().toString(),
                team2.getTeamType().name(),
                Set.of(RegulatorTeamRole.SCAP_VIEWER.name())
            ),
            new ServiceProviderUserTeamRolesDto(
                2L,
                team2.getUuid().toString(),
                team2.getTeamType().name(),
                Set.of(IndustryTeamRole.ACCESS_MANAGER.name(), IndustryTeamRole.SCAP_VIEWER.name())
            )
        );
  }

  @Test
  void belongsToAnyTeam_returnsTrue_whenUserHasTeamRoles() {
    when(teamMemberRoleRepository.existsByWuaId(300165L)).thenReturn(true);

    assertThat(energyPortalDataService.belongsToAnyTeam(300165L)).isTrue();
  }

  @Test
  void belongsToAnyTeam_returnsFalse_whenUserHasNoTeamRoles() {
    when(teamMemberRoleRepository.existsByWuaId(300165L)).thenReturn(false);

    assertThat(energyPortalDataService.belongsToAnyTeam(300165L)).isFalse();
  }

  private ServiceProviderTeamTypeRoleDto createServiceRoleDto(TeamRole role, boolean isAssessManager) {
    return new ServiceProviderTeamTypeRoleDto(
        role.name(),
        role.getDisplayName(),
        role.getDescription(),
        isAssessManager,
        role.getDisplayOrder()
    );
  }
}