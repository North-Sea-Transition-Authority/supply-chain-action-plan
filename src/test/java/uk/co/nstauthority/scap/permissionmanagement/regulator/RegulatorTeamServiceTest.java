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
  private TeamRepository teamRepository;

  @Mock
  private TeamMemberService teamMemberService;

  @Mock
  private TeamMemberRoleService teamMemberRoleService;

  @Mock
  private NewTeamFormvalidator newTeamFormvalidator;

  private RegulatorTeamService regulatorTeamService;

  @BeforeEach
  void setup() {
    regulatorTeamService = new RegulatorTeamService(teamMemberService, teamRepository, teamMemberRoleService, newTeamFormvalidator);
  }

  @Test
  void getRegulatorTeamForUser_whenUserBelongsToRegulatorTeam_thenReturnRegulatorTeam() {
    var user = ServiceUserDetailTestUtil.Builder().build();
    var team = new Team();
    when(teamRepository.findAllTeamsOfTypeThatUserIsMemberOf(user.wuaId(), TeamType.REGULATOR))
        .thenReturn(List.of(team));

    var result = regulatorTeamService.getTeamsOfTypeThatUserBelongsTo(user, TeamType.REGULATOR);
    assertThat(result).isEqualTo(List.of(team));
  }

  @Test
  void getRegulatorTeamForUser_whenUserDoesNotBelongToRegulatorTeam_thenReturnEmpty() {
    var user = ServiceUserDetailTestUtil.Builder().build();
    when(teamRepository.findAllTeamsOfTypeThatUserIsMemberOf(user.wuaId(), TeamType.REGULATOR)).thenReturn(List.of());

    var result = regulatorTeamService.getTeamsOfTypeThatUserBelongsTo(user, TeamType.REGULATOR);

    assertThat(result).isEmpty();
  }

  @Test
  void getTeam_whenMatch_thenReturnTeam() {

    var team = TeamTestUtil.Builder().build();

    var teamId = new TeamId(team.getUuid());

    when(teamRepository.findByUuid(teamId.uuid())).thenReturn(Optional.of(team));

    assertThat(regulatorTeamService.getTeam(teamId)).isEqualTo(team);
    verify(teamRepository).findByUuid(teamId.uuid());
  }

  @Test
  void getTeam_whenNoMatch_thenEmptyOptional() {

    var team = TeamTestUtil.Builder().build();

    var teamId = new TeamId(team.getUuid());

    when(teamRepository.findByUuid(teamId.uuid())).thenReturn(Optional.empty());

    assertThat(regulatorTeamService.findTeam(teamId)).isEmpty();
    verify(teamRepository).findByUuid(teamId.uuid());
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

  @Test
  void addUserTeamRoles_verifyRepositoryInteractions() {

    var team = TeamTestUtil.Builder().build();
    var userToAdd = EnergyPortalUserDtoTestUtil.Builder().build();
    var regulatorRoles = Set.of(
        RegulatorTeamRole.ACCESS_MANAGER,
        RegulatorTeamRole.ORGANISATION_ACCESS_MANAGER
    );

    regulatorTeamService.addUserTeamRoles(team, userToAdd, regulatorRoles);

    var rolesAsStrings = regulatorRoles
        .stream()
        .map(RegulatorTeamRole::name)
        .collect(Collectors.toSet());

    verify(teamMemberRoleService).addUserTeamRoles(team, userToAdd, rolesAsStrings);
  }
}