package uk.co.nstauthority.scap.permissionmanagement.industry;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;

import java.util.List;
import java.util.Set;
import java.util.UUID;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.authentication.ServiceUserDetailTestUtil;
import uk.co.nstauthority.scap.permissionmanagement.Team;
import uk.co.nstauthority.scap.permissionmanagement.TeamId;
import uk.co.nstauthority.scap.permissionmanagement.TeamTestUtil;
import uk.co.nstauthority.scap.permissionmanagement.TeamType;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberService;
import uk.co.nstauthority.scap.permissionmanagement.regulator.RegulatorTeamRole;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;

@ExtendWith(MockitoExtension.class)
class IndustryTeamServiceTest {

  @Mock
  private TeamMemberService teamMemberService;

  @Mock
  private TeamService teamService;

  private IndustryTeamService industryTeamService;

  @BeforeEach
  void setup() {
    industryTeamService = new IndustryTeamService(teamMemberService, teamService);
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

  @Test
  void isAccessManager_whenOrganisationAccessManager_thenTrue() {
    var teamId = new TeamId(UUID.randomUUID());
    var user = ServiceUserDetailTestUtil.Builder().build();

    when(teamMemberService.isMemberOfTeamWithAnyRoleOf(teamId, user, Set.of(RegulatorTeamRole.ACCESS_MANAGER.name())))
        .thenReturn(false);
    when(teamService.getTeamsOfTypeThatUserBelongsTo(user, TeamType.REGULATOR)).thenReturn(getRegulatorTeam());
    when(teamMemberService
        .isMemberOfTeamWithAnyRoleOf(any(TeamId.class),
            eq(user),
            eq(Set.of(RegulatorTeamRole.ORGANISATION_ACCESS_MANAGER.name()))))
        .thenReturn(true);
    assertTrue(industryTeamService.isAccessManager(teamId, user));
  }

  private List<Team> getRegulatorTeam() {
    return List.of(
        TeamTestUtil.Builder()
        .withTeamType(TeamType.REGULATOR)
        .build());
  }
}