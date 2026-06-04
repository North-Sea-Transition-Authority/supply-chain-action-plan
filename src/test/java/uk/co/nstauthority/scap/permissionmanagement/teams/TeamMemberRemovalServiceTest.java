package uk.co.nstauthority.scap.permissionmanagement.teams;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.fivium.energyportal.starter.accounts.EnergyPortalServiceAccessService;
import uk.co.fivium.energyportal.starter.serviceproviders.EnergyPortalAccountsMessagePublishingService;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberTestUtil;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberViewTestUtil;
import uk.co.nstauthority.scap.permissionmanagement.TeamTestUtil;
import uk.co.nstauthority.scap.permissionmanagement.industry.IndustryTeamRole;
import uk.co.nstauthority.scap.permissionmanagement.regulator.RegulatorTeamRole;

@ExtendWith(MockitoExtension.class)
class TeamMemberRemovalServiceTest {

  @Mock
  TeamMemberService teamMemberService;

  @Mock
  TeamMemberRoleRepository teamMemberRoleRepository;

  @Mock
  EnergyPortalAccountsMessagePublishingService energyPortalAccountsMessagePublishingService;

  @Mock
  EnergyPortalServiceAccessService energyPortalServiceAccessService;

  @InjectMocks
  private TeamMemberRemovalService teamMemberRemovalService;

  @Test
  void removeTeamMember_lastAccessManager_IllegalStateException() {
    var team = TeamTestUtil.Builder().build();
    var user = TeamMemberTestUtil
        .Builder()
        .withRole(RegulatorTeamRole.ACCESS_MANAGER)
        .build();

    when(teamMemberService.getTeamMembers(team)).thenReturn(List.of(user));
    assertThrows(IllegalStateException.class, () -> teamMemberRemovalService.removeTeamMember(team, user));
    verify(energyPortalAccountsMessagePublishingService, never()).publishRemoveUserFromTeam(
        anyLong(),
        any()
    );
    verifyNoInteractions(energyPortalServiceAccessService);
  }

  @Test
  void removeTeamMember_notLastAccessManager_returns() {
    var team = TeamTestUtil.Builder().build();
    var user1 = TeamMemberTestUtil
        .Builder()
        .withRole(RegulatorTeamRole.ACCESS_MANAGER)
        .withWebUserAccountId(1000)
        .build();
    var user2 = TeamMemberTestUtil
        .Builder()
        .withRole(RegulatorTeamRole.ACCESS_MANAGER)
        .withWebUserAccountId(2000)
        .build();

    when(teamMemberService.getTeamMembers(team)).thenReturn(List.of(user1, user2));
    when(teamMemberService.getAllPermissionsForUser(user1.wuaId().id())).thenReturn(List.of(RolePermission.GRANT_ROLES));
    teamMemberRemovalService.removeTeamMember(team, user1);
    verify(teamMemberRoleRepository).findAllByTeamAndWuaId(team, user1.wuaId().id());
    verify(energyPortalAccountsMessagePublishingService).publishRemoveUserFromTeam(
        1000L,
        team.getUuid().toString()
    );
    verifyNoInteractions(energyPortalServiceAccessService);
  }

  @Test
  void removeTeamMember_whenNoMoreRoles_thenRemoveAccess() {
    var team = TeamTestUtil.Builder().build();
    var user1 = TeamMemberTestUtil
        .Builder()
        .withRole(RegulatorTeamRole.ACCESS_MANAGER)
        .withWebUserAccountId(1000)
        .build();
    var user2 = TeamMemberTestUtil
        .Builder()
        .withRole(RegulatorTeamRole.ACCESS_MANAGER)
        .withWebUserAccountId(2000)
        .build();

    when(teamMemberService.getTeamMembers(team)).thenReturn(List.of(user1, user2));
    when(teamMemberService.getAllPermissionsForUser(user1.wuaId().id())).thenReturn(List.of());
    teamMemberRemovalService.removeTeamMember(team, user1);
    verify(teamMemberRoleRepository).findAllByTeamAndWuaId(team, user1.wuaId().id());
    verify(energyPortalAccountsMessagePublishingService).publishRemoveUserFromTeam(
        1000L,
        team.getUuid().toString()
    );
    verify(energyPortalServiceAccessService).removeUser(user1.wuaId().id());
  }

  @Test
  void removeTeamMember_notLastOrganisationAccessManager_returns() {
    var team = TeamTestUtil.Builder().build();
    var user1 = TeamMemberTestUtil
        .Builder()
        .withRole(IndustryTeamRole.ACCESS_MANAGER)
        .withWebUserAccountId(1000)
        .build();
    var user2 = TeamMemberTestUtil
        .Builder()
        .withRole(RegulatorTeamRole.ORGANISATION_ACCESS_MANAGER)
        .withWebUserAccountId(2000)
        .build();

    when(teamMemberService.getTeamMembers(team)).thenReturn(List.of(user1, user2));
    when(teamMemberService.getAllPermissionsForUser(user1.wuaId().id())).thenReturn(List.of(RolePermission.GRANT_ROLES));
    teamMemberRemovalService.removeTeamMember(team, user1);
    verify(teamMemberRoleRepository).findAllByTeamAndWuaId(team, user1.wuaId().id());
    verify(energyPortalAccountsMessagePublishingService).publishRemoveUserFromTeam(
        1000L,
        team.getUuid().toString()
    );
    verifyNoInteractions(energyPortalServiceAccessService);
  }

  @Test
  void getRemoveScreenPageTitle_lastAccessManager_cannotRemoveUserTitle() {
    var teamMemberView = TeamMemberViewTestUtil.Builder().build();

    var title = teamMemberRemovalService.getRemoveScreenPageTitle("TestTeam", teamMemberView, false);
    assertThat(title).isEqualTo("Unable to remove %s from %s".formatted(teamMemberView.getDisplayName(), "TestTeam"));
  }

  @Test
  void getRemoveScreenPageTitle_notLastAccessManager_canRemoveUserTitle() {
    var teamMemberView = TeamMemberViewTestUtil.Builder().build();

    var title = teamMemberRemovalService.getRemoveScreenPageTitle("TestTeam", teamMemberView, true);
    assertThat(title).contains("Are you sure you want to remove");
  }
}
