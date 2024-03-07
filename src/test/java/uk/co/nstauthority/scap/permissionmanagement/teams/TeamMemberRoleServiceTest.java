package uk.co.nstauthority.scap.permissionmanagement.teams;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.tuple;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

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
import uk.co.nstauthority.scap.authentication.ServiceUserDetailTestUtil;
import uk.co.nstauthority.scap.authentication.UserDetailService;
import uk.co.nstauthority.scap.permissionmanagement.Team;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberTestUtil;
import uk.co.nstauthority.scap.permissionmanagement.TeamTestUtil;
import uk.co.nstauthority.scap.utils.EnergyPortalUserDtoTestUtil;

@ExtendWith(MockitoExtension.class)
class TeamMemberRoleServiceTest {

  @Mock
  private TeamMemberRoleRepository teamMemberRoleRepository;

  @Mock
  private UserDetailService userDetailService;

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
    var instigatingUser = ServiceUserDetailTestUtil.Builder()
        .withWuaId(200L)
        .build();

    teamMemberRoleService.updateUserTeamRoles(team, existingUser.wuaId().id(), Set.of(role));

    verify(teamMemberRoleRepository).deleteAllByTeamAndWuaId(team, existingUser.wuaId().id());
    verify(teamMemberRoleRepository).saveAll(teamMemberRoleCaptor.capture());

    Assertions.assertThat(teamMemberRoleCaptor.getValue())
        .extracting(TeamMemberRole::getTeam, TeamMemberRole::getWuaId, TeamMemberRole::getRole)
        .containsExactly(tuple(team, existingUser.wuaId().id(), role));
  }

  @Test
  void updateUserTeamRoles_whenMemberWithMultipleRoles_thenVerifyMultipleRowInsert() {

    var team = TeamTestUtil.Builder().build();
    var existingUser = TeamMemberTestUtil.Builder()
        .withWebUserAccountId(100)
        .build();
    var instigatingUser = ServiceUserDetailTestUtil.Builder()
        .withWuaId(200L)
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
  }

  @Test
  void deleteAllByTeam_verifyCalls() {
    var team = new Team();
    teamMemberRoleService.deleteUsersInTeam(team);

    verify(teamMemberRoleRepository).deleteAllByTeam(team);
  }
}