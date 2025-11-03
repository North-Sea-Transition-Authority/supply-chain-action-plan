package uk.co.nstauthority.scap.permissionmanagement.teams;

import static org.assertj.core.api.Assertions.tuple;
import static org.mockito.Mockito.verify;
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
import uk.co.fivium.energyportal.starter.serviceproviders.EnergyPortalServiceProviderUserRolesService;
import uk.co.nstauthority.scap.authentication.UserDetailService;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberTestUtil;
import uk.co.nstauthority.scap.permissionmanagement.TeamTestUtil;
import uk.co.nstauthority.scap.utils.EnergyPortalUserDtoTestUtil;

@ExtendWith(MockitoExtension.class)
class TeamMemberRoleServiceTest {

  @Mock
  private TeamMemberRoleRepository teamMemberRoleRepository;

  @Mock
  private UserDetailService userDetailService;

  @Mock
  private EnergyPortalServiceProviderUserRolesService energyPortalServiceProviderUserRolesService;

  @InjectMocks
  private TeamMemberRoleService teamMemberRoleService;

  @Captor
  private ArgumentCaptor<List<TeamMemberRole>> teamMemberRoleCaptor;

  @Test
  void addUserTeamRoles_whenAddingUser_thenVerifyCalls() {
    var team = TeamTestUtil.Builder().build();
    var userToAdd = EnergyPortalUserDtoTestUtil.Builder()
        .withWebUserAccountId(100)
        .build();
    var role = "ROLE_NAME";

    teamMemberRoleService.addUserTeamRoles(team, userToAdd, Set.of(role));

    verify(teamMemberRoleRepository).deleteAllByTeamAndWuaId(team, userToAdd.webUserAccountId());
    verify(teamMemberRoleRepository).saveAll(teamMemberRoleCaptor.capture());

    Assertions.assertThat(teamMemberRoleCaptor.getValue())
        .extracting(TeamMemberRole::getTeam, TeamMemberRole::getWuaId, TeamMemberRole::getRole)
        .containsExactly(tuple(team, userToAdd.webUserAccountId(), role));
  }

  @Test
  void addUserTeamRoles_whenAddingUserWithId_thenVerifyCalls() {
    var team = TeamTestUtil.Builder().build();
    var userToAdd = EnergyPortalUserDtoTestUtil.Builder()
        .withWebUserAccountId(100)
        .build();
    var role = "ROLE_NAME";

    teamMemberRoleService.addUserTeamRoles(team, 100L, Set.of(role));

    verify(teamMemberRoleRepository).deleteAllByTeamAndWuaId(team, userToAdd.webUserAccountId());
    verify(teamMemberRoleRepository).saveAll(teamMemberRoleCaptor.capture());

    Assertions.assertThat(teamMemberRoleCaptor.getValue())
        .extracting(TeamMemberRole::getTeam, TeamMemberRole::getWuaId, TeamMemberRole::getRole)
        .containsExactly(tuple(team, userToAdd.webUserAccountId(), role));
  }

  @Test
  void updateUserTeamRoles_whenMemberWithOneRole_thenVerifySingleRowInsert() {
    var team = TeamTestUtil.Builder().build();
    var existingUser = TeamMemberTestUtil.Builder()
        .withWebUserAccountId(100)
        .build();
    var role = "ROLE_NAME";

    teamMemberRoleService.updateUserTeamRoles(team, existingUser.wuaId().id(), Set.of(role));

    verify(teamMemberRoleRepository).deleteAllByTeamAndWuaId(team, existingUser.wuaId().id());
    verify(teamMemberRoleRepository).saveAll(teamMemberRoleCaptor.capture());

    Assertions.assertThat(teamMemberRoleCaptor.getValue())
        .extracting(TeamMemberRole::getTeam, TeamMemberRole::getWuaId, TeamMemberRole::getRole)
        .containsExactly(tuple(team, existingUser.wuaId().id(), role));

    verify(energyPortalServiceProviderUserRolesService).publishUsersRolesForTeam(
        existingUser.wuaId().id(),
        team.getUuid().toString(),
        team.getTeamType().name(),
        Set.of(role)
    );
  }

  @Test
  void updateUserTeamRoles_whenMemberWithMultipleRoles_thenVerifyMultipleRowInsert() {

    var team = TeamTestUtil.Builder().build();
    var existingUser = TeamMemberTestUtil.Builder()
        .withWebUserAccountId(100)
        .build();

    var firstRole = "FIRST_ROLE_NAME";
    var secondRole = "SECOND_ROLE_NAME";

    var rolesToGrant = Set.of(firstRole, secondRole);

    teamMemberRoleService.updateUserTeamRoles(team, existingUser.wuaId().id(), rolesToGrant);

    verify(teamMemberRoleRepository).deleteAllByTeamAndWuaId(team, existingUser.wuaId().id());
    verify(teamMemberRoleRepository).saveAll(teamMemberRoleCaptor.capture());

    Assertions.assertThat(teamMemberRoleCaptor.getValue())
        .extracting(TeamMemberRole::getTeam, TeamMemberRole::getWuaId, TeamMemberRole::getRole)
        .containsExactlyInAnyOrder(
            tuple(team, existingUser.wuaId().id(), firstRole),
            tuple(team, existingUser.wuaId().id(), secondRole)
        );

    verify(teamMemberRoleRepository).deleteAllByTeamAndWuaId(team, existingUser.wuaId().id());
    verify(energyPortalServiceProviderUserRolesService).publishUsersRolesForTeam(
        existingUser.wuaId().id(),
        team.getUuid().toString(),
        team.getTeamType().name(),
        Set.of(firstRole, secondRole)
    );
  }

  @Test
  void deleteAllByTeam_verifyCalls() {
    var team = TeamTestUtil.Builder().build();
    var teamMemberRole = TeamMemberRoleTestUtil.Builder().build();

    when(teamMemberRoleRepository.findAllByTeam(team)).thenReturn(List.of(teamMemberRole));

    teamMemberRoleService.deleteUsersInTeam(team);

    verify(teamMemberRoleRepository).deleteAllByTeam(team);
    verify(energyPortalServiceProviderUserRolesService).publishRemoveUserFromTeam(
        teamMemberRole.getWuaId(),
        team.getUuid().toString()
    );
  }
}