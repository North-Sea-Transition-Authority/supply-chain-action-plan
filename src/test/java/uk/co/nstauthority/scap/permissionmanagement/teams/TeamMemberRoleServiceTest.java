package uk.co.nstauthority.scap.permissionmanagement.teams;

import static org.assertj.core.api.Assertions.tuple;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import java.util.List;
import java.util.Set;
import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.fivium.energyportal.starter.accounts.EnergyPortalServiceAccessService;
import uk.co.fivium.energyportal.starter.serviceproviders.EnergyPortalAccountsMessagePublishingService;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.permissionmanagement.TeamTestUtil;

@ExtendWith(MockitoExtension.class)
class TeamMemberRoleServiceTest {

  @Mock
  private TeamMemberRoleRepository teamMemberRoleRepository;

  @Mock
  private EnergyPortalAccountsMessagePublishingService energyPortalAccountsMessagePublishingService;

  @Mock
  private TeamMemberService teamMemberService;

  @Mock
  private EnergyPortalServiceAccessService energyPortalServiceAccessService;

  @InjectMocks
  private TeamMemberRoleService teamMemberRoleService;

  @Captor
  private ArgumentCaptor<List<TeamMemberRole>> teamMemberRoleCaptor;

  @Test
  void updateUserTeamRoles_whenMemberWithOneRole_thenVerifySingleRowInsert() {
    var team = TeamTestUtil.Builder().build();
    var wuaId = 100L;
    var role = "ROLE_NAME";

    when(teamMemberService.getAllPermissionsForUser(wuaId)).thenReturn(List.of());

    teamMemberRoleService.updateUserTeamRoles(team, wuaId, Set.of(role));

    verify(teamMemberRoleRepository).deleteAllByTeamAndWuaId(team, wuaId);
    verify(teamMemberRoleRepository).saveAll(teamMemberRoleCaptor.capture());

    Assertions.assertThat(teamMemberRoleCaptor.getValue())
        .extracting(TeamMemberRole::getTeam, TeamMemberRole::getWuaId, TeamMemberRole::getRole)
        .containsExactly(tuple(team, wuaId, role));
    verify(energyPortalServiceAccessService).addUser(wuaId);
    verify(energyPortalAccountsMessagePublishingService).publishUsersRolesForTeam(
        wuaId,
        team.getUuid().toString(),
        team.getTeamType().name(),
        Set.of(role)
    );
  }

  @Test
  void updateUserTeamRoles_whenMemberWithMultipleRoles_thenVerifyMultipleRowInsert() {

    var team = TeamTestUtil.Builder().build();
    var wuaId = 100L;
    var firstRole = "FIRST_ROLE_NAME";
    var secondRole = "SECOND_ROLE_NAME";

    var rolesToGrant = Set.of(firstRole, secondRole);

    teamMemberRoleService.updateUserTeamRoles(team, wuaId, rolesToGrant);

    verify(teamMemberRoleRepository).deleteAllByTeamAndWuaId(team, wuaId);
    verify(teamMemberRoleRepository).saveAll(teamMemberRoleCaptor.capture());

    Assertions.assertThat(teamMemberRoleCaptor.getValue())
        .extracting(TeamMemberRole::getTeam, TeamMemberRole::getWuaId, TeamMemberRole::getRole)
        .containsExactlyInAnyOrder(
            tuple(team, wuaId, firstRole),
            tuple(team, wuaId, secondRole)
        );


    verify(energyPortalServiceAccessService).addUser(wuaId);
    verify(teamMemberRoleRepository).deleteAllByTeamAndWuaId(team, wuaId);
    verify(energyPortalAccountsMessagePublishingService).publishUsersRolesForTeam(
        wuaId,
        team.getUuid().toString(),
        team.getTeamType().name(),
        Set.of(firstRole, secondRole)
    );
  }

  @Test
  void updateUserTeamRoles_whenUserAlreadyHadRoles_thenDontCallServiceAccessService() {

    var team = TeamTestUtil.Builder().build();
    var wuaId = 100L;
    var firstRole = "FIRST_ROLE_NAME";

    var rolesToGrant = Set.of(firstRole);

    when(teamMemberService.getAllPermissionsForUser(wuaId)).thenReturn(List.of(RolePermission.GRANT_ROLES));

    teamMemberRoleService.updateUserTeamRoles(team, wuaId, rolesToGrant);

    verify(teamMemberRoleRepository).deleteAllByTeamAndWuaId(team, wuaId);
    verify(teamMemberRoleRepository).saveAll(teamMemberRoleCaptor.capture());

    Assertions.assertThat(teamMemberRoleCaptor.getValue())
        .extracting(TeamMemberRole::getTeam, TeamMemberRole::getWuaId, TeamMemberRole::getRole)
        .containsExactlyInAnyOrder(
            tuple(team, wuaId, firstRole)
        );

    verifyNoInteractions(energyPortalServiceAccessService);
    verify(teamMemberRoleRepository).deleteAllByTeamAndWuaId(team, wuaId);
    verify(energyPortalAccountsMessagePublishingService).publishUsersRolesForTeam(
        wuaId,
        team.getUuid().toString(),
        team.getTeamType().name(),
        Set.of(firstRole)
    );
  }

  @Test
  void deleteAllByTeam() {
    var team = TeamTestUtil.Builder().build();
    var teamMemberRole1 = TeamMemberRoleTestUtil.Builder().withWebUserAccountId(1).build();
    var teamMemberRole2 = TeamMemberRoleTestUtil.Builder().withWebUserAccountId(1).build();
    var teamMemberRole3 = TeamMemberRoleTestUtil.Builder().withWebUserAccountId(2).build();


    when(teamMemberRoleRepository.findAllByTeam(team)).thenReturn(List.of(teamMemberRole1, teamMemberRole3));

    when(teamMemberRoleRepository.findAllByWuaIdIn(Set.of(1L, 2L))).thenReturn(List.of(teamMemberRole2));

    teamMemberRoleService.deleteUsersInTeam(team);

    verify(teamMemberRoleRepository).deleteAllByTeam(team);
    verify(energyPortalServiceAccessService, times(1)).removeUser(anyLong());
    verify(energyPortalServiceAccessService).removeUser(2L);
    verify(energyPortalAccountsMessagePublishingService).publishRemoveUserFromTeam(
        2L,
        team.getUuid().toString()
    );
  }
}