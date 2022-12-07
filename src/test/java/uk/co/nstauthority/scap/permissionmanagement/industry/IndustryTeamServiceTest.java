package uk.co.nstauthority.scap.permissionmanagement.industry;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.authentication.ServiceUserDetailTestUtil;
import uk.co.nstauthority.scap.permissionmanagement.TeamId;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberRoleService;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberService;
import uk.co.nstauthority.scap.permissionmanagement.TeamTestUtil;
import uk.co.nstauthority.scap.permissionmanagement.TeamType;
import uk.co.nstauthority.scap.permissionmanagement.regulator.RegulatorTeamRole;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;

@ExtendWith(MockitoExtension.class)
class IndustryTeamServiceTest {

  @Mock
  private TeamService teamService;

  @Mock
  private TeamMemberService teamMemberService;

  @Mock
  private TeamMemberRoleService teamMemberRoleService;

  @InjectMocks
  private IndustryTeamService industryTeamService;

  @Test
  void getTeam_whenMatch_thenReturnTeam() {

    var team = TeamTestUtil
        .Builder()
        .withTeamType(TeamType.INDUSTRY)
        .build();

    var teamId = new TeamId(team.getUuid());

    when(teamService.getTeam(teamId, TeamType.INDUSTRY)).thenReturn(Optional.of(team));

    assertThat(industryTeamService.getTeam(teamId)).contains(team);
    verify(teamService, times(1)).getTeam(teamId, team.getTeamType());
  }

  @Test
  void getTeam_whenNoMatch_thenEmptyOptional() {

    var team = TeamTestUtil
        .Builder()
        .withTeamType(TeamType.INDUSTRY)
        .build();

    var teamId = new TeamId(team.getUuid());

    when(teamService.getTeam(teamId, TeamType.INDUSTRY)).thenReturn(Optional.empty());

    assertThat(industryTeamService.getTeam(teamId)).isEmpty();
    verify(teamService, times(1)).getTeam(teamId, team.getTeamType());
  }

  @Test
  void isAccessManager_whenAccessManager_thenTrue() {

    var teamId = new TeamId(UUID.randomUUID());
    var user = ServiceUserDetailTestUtil.Builder().build();

    when(teamMemberService.isMemberOfTeamWithAnyRoleOf(teamId, user, Set.of(RegulatorTeamRole.ACCESS_MANAGER.name())))
        .thenReturn(true);

    assertTrue(industryTeamService.isAccessManager(teamId, user));
  }

  @Test
  void isAccessManager_whenAccessManager_thenFalse() {

    var teamId = new TeamId(UUID.randomUUID());
    var user = ServiceUserDetailTestUtil.Builder().build();

    when(teamMemberService.isMemberOfTeamWithAnyRoleOf(teamId, user, Set.of(RegulatorTeamRole.ACCESS_MANAGER.name())))
        .thenReturn(false);

    assertFalse(industryTeamService.isAccessManager(teamId, user));
  }
}