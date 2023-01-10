package uk.co.nstauthority.scap.permissionmanagement.teams;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
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


  private TeamMemberRemovalService teamMemberRemovalService;

  @BeforeEach
  void setup() {
    teamMemberRemovalService = new TeamMemberRemovalService(teamMemberService, teamMemberRoleRepository);
  }

  @Test
  void removeTeamMember_lastAccessManager_IllegalStateException() {
    var team = TeamTestUtil.Builder().build();
    var user = TeamMemberTestUtil
        .Builder()
        .withRole(RegulatorTeamRole.ACCESS_MANAGER)
        .build();

    when(teamMemberService.getTeamMembers(team)).thenReturn(List.of(user));
    assertThrows(IllegalStateException.class, () -> teamMemberRemovalService.removeTeamMember(team, user));
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
    teamMemberRemovalService.removeTeamMember(team, user1);
    verify(teamMemberRoleRepository).findAllByTeamAndWuaId(team, user1.wuaId().id());
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
    teamMemberRemovalService.removeTeamMember(team, user1);
    verify(teamMemberRoleRepository).findAllByTeamAndWuaId(team, user1.wuaId().id());
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
