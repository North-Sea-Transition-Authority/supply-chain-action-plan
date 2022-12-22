package uk.co.nstauthority.scap.permissionmanagement.regulator;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.authentication.ServiceUserDetailTestUtil;
import uk.co.nstauthority.scap.error.exception.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.permissionmanagement.Team;
import uk.co.nstauthority.scap.permissionmanagement.TeamId;
import uk.co.nstauthority.scap.permissionmanagement.TeamRepository;
import uk.co.nstauthority.scap.permissionmanagement.teams.NewTeamFormvalidator;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberRoleService;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberService;
import uk.co.nstauthority.scap.permissionmanagement.TeamType;
import uk.co.nstauthority.scap.permissionmanagement.TeamTestUtil;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;
import uk.co.nstauthority.scap.utils.EnergyPortalUserDtoTestUtil;

@ExtendWith(MockitoExtension.class)
class RegulatorTeamServiceTest {

  @Mock
  private TeamMemberService teamMemberService;

  @Mock
  private TeamService teamService;

  private RegulatorTeamService regulatorTeamService;

  @BeforeEach
  void setup() {
    regulatorTeamService = new RegulatorTeamService(teamMemberService, teamService);
  }

  @Test
  void getRegulatorTeamForUser_whenUserBelongsToRegulatorTeam_thenReturnRegulatorTeam() {
    var user = ServiceUserDetailTestUtil.Builder().build();
    var team = new Team();
    when(teamService.getTeamsOfTypeThatUserBelongsTo(user, TeamType.REGULATOR))
        .thenReturn(List.of(team));

    var result = regulatorTeamService.getTeam(user);
    assertThat(result).isEqualTo(team);
  }

  @Test
  void isAccessManager_whenAccessManager_thenTrue() {

    var teamId = new TeamId(UUID.randomUUID());
    var user = ServiceUserDetailTestUtil.Builder().build();

    when(teamMemberService.isMemberOfTeamWithAnyRoleOf(teamId, user, Set.of(RegulatorTeamRole.ACCESS_MANAGER.name())))
        .thenReturn(true);

    assertTrue(regulatorTeamService.isAccessManager(teamId, user));
  }

  @Test
  void isAccessManager_whenAccessManager_thenFalse() {

    var teamId = new TeamId(UUID.randomUUID());
    var user = ServiceUserDetailTestUtil.Builder().build();

    when(teamMemberService.isMemberOfTeamWithAnyRoleOf(teamId, user, Set.of(RegulatorTeamRole.ACCESS_MANAGER.name())))
        .thenReturn(false);

    assertFalse(regulatorTeamService.isAccessManager(teamId, user));
  }
}