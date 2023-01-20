package uk.co.nstauthority.scap.permissionmanagement.teams;

import static java.util.Collections.emptyList;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import java.util.List;
import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.authentication.ServiceUserDetailTestUtil;
import uk.co.nstauthority.scap.permissionmanagement.Team;
import uk.co.nstauthority.scap.permissionmanagement.TeamId;
import uk.co.nstauthority.scap.permissionmanagement.TeamRepository;
import uk.co.nstauthority.scap.permissionmanagement.TeamTestUtil;
import uk.co.nstauthority.scap.permissionmanagement.TeamType;

@ExtendWith(MockitoExtension.class)
class TeamServiceTest {

  @Mock
  private TeamRepository teamRepository;

  @Mock
  private TeamMemberRoleService teamMemberRoleService;

  @InjectMocks
  private TeamService teamService;

  @Test
  void getTeam_whenMatch_thenReturnTeam() {

    var team = TeamTestUtil.Builder().build();

    when(teamRepository.findByUuid(team.getUuid())).thenReturn(Optional.of(team));
    var result = teamService.getTeam(TeamId.valueOf(team.getUuid()));

    assertThat(result).isEqualTo(team);
    verify(teamRepository).findByUuid(team.getUuid());
  }

  @Test
  void findTeam_whenNoMatch_thenEmptyOptionalReturned() {

    var team = TeamTestUtil.Builder().build();

    when(teamRepository.findByUuid(team.getUuid())).thenReturn(Optional.empty());
    var result = teamService.findTeam(TeamId.valueOf(team.getUuid()));

    assertThat(result).isEmpty();
    verify(teamRepository).findByUuid(team.getUuid());
  }

  @ParameterizedTest
  @EnumSource(value = TeamType.class)
  void getTeamsOfTypeThatUserBelongsTo_whenUserIsNotMember_thenNoTeamsReturned(TeamType teamType) {
    var user = ServiceUserDetailTestUtil.Builder().build();
    when(teamRepository.findAllTeamsOfTypeThatUserIsMemberOf(user.wuaId(), teamType)).thenReturn(List.of());

    var result = teamService.getTeamsOfTypeThatUserBelongsTo(user, teamType);

    assertThat(result).isEmpty();
    verify(teamRepository).findAllTeamsOfTypeThatUserIsMemberOf(user.wuaId(), teamType);
  }

  @ParameterizedTest
  @EnumSource(value = TeamType.class)
  void getTeamsOfTypeThatUserBelongsTo_whenUserIsMember_thenTeamsReturned(TeamType teamType) {
    var user = ServiceUserDetailTestUtil.Builder().build();
    var team = new Team();

    when(teamRepository.findAllTeamsOfTypeThatUserIsMemberOf(user.wuaId(), teamType)).thenReturn(List.of(team));

    var result = teamService.getTeamsOfTypeThatUserBelongsTo(user, teamType);

    assertThat(result).containsExactly(team);
    verify(teamRepository).findAllTeamsOfTypeThatUserIsMemberOf(user.wuaId(), teamType);
  }

  @Test
  void getTeamsThatUserBelongsTo_Matches_ReturnsTeamsOfAllTypes() {
    var user = ServiceUserDetailTestUtil.Builder().build();
    var team1 = new Team();
    team1.setTeamType(TeamType.REGULATOR);
    var team2 = new Team();
    team2.setTeamType(TeamType.INDUSTRY);

    when(teamRepository.findAllTeamsThatUserIsMemberOf(user.wuaId())).thenReturn(List.of(team1, team2));

    var result = teamService.getTeamsThatUserBelongsTo(user);

    assertThat(result)
        .contains(team1)
        .contains(team2);
    verify(teamRepository).findAllTeamsThatUserIsMemberOf(user.wuaId());
  }

  @Test
  void getTeamsThatUserBelongsTo_NoMatch_ReturnsEmptyList() {
    var user = ServiceUserDetailTestUtil.Builder().build();
    when(teamRepository.findAllTeamsThatUserIsMemberOf(user.wuaId())).thenReturn(List.of());

    var result = teamService.getTeamsThatUserBelongsTo(user);

    assertThat(result).isEmpty();
    verify(teamRepository).findAllTeamsThatUserIsMemberOf(user.wuaId());
  }

  @Test
  void findTeamByOrgGroupId_NoMatch_EmptyOptional() {
    var team = TeamTestUtil
        .Builder()
        .withTeamType(TeamType.INDUSTRY)
        .build();

    when(teamRepository.findByEnergyPortalOrgGroupId(team.getEnergyPortalOrgGroupId())).thenReturn(Optional.empty());
    var result = teamService.findByEnergyPortalOrgGroupId(team.getEnergyPortalOrgGroupId());

    assertThat(result).isEmpty();
    verify(teamRepository).findByEnergyPortalOrgGroupId(team.getEnergyPortalOrgGroupId());
  }

  @Test
  void findTeamByOrgGroupId_Match_TeamReturend() {
    var team = TeamTestUtil
        .Builder()
        .withTeamType(TeamType.INDUSTRY)
        .build();

    when(teamRepository.findByEnergyPortalOrgGroupId(team.getEnergyPortalOrgGroupId())).thenReturn(Optional.of(team));
    var result = teamService.findByEnergyPortalOrgGroupId(team.getEnergyPortalOrgGroupId());

    assertThat(result).contains(team);
    verify(teamRepository).findByEnergyPortalOrgGroupId(team.getEnergyPortalOrgGroupId());
  }

  @Test
  void createTeam_newTeam_newAccessManager() {
    var team = TeamTestUtil
        .Builder()
        .withTeamType(TeamType.INDUSTRY)
        .build();
    var teamCaptor = ArgumentCaptor.forClass(Team.class);

    when(teamRepository.save(any())).thenReturn(team);
    var result = teamService.createTeam(team.getDisplayName(), team.getEnergyPortalOrgGroupId());


    verify(teamRepository).save(teamCaptor.capture());
    verifyNoInteractions(teamMemberRoleService);

    var capturedTeam = teamCaptor.getAllValues().get(0);
    assertThat(result.getDisplayName()).isEqualTo(team.getDisplayName());
    assertThat(capturedTeam.getDisplayName()).isEqualTo(team.getDisplayName());
    assertThat(capturedTeam.getEnergyPortalOrgGroupId()).isEqualTo(team.getEnergyPortalOrgGroupId());
  }

  @Test
  void userIsMemberofTeam_notMemberofTeam() {
    var user = ServiceUserDetailTestUtil.Builder().build();
    when(teamRepository.findAllTeamsThatUserIsMemberOf(user.wuaId())).thenReturn(emptyList());
    assertFalse(teamService.userIsMemberOfOrganisationGroupTeam(1000, user));
  }

  @Test
  void userIsMemberofTeam_MemberofTeam() {
    var user = ServiceUserDetailTestUtil.Builder().build();
    var team = TeamTestUtil.Builder().withTeamType(TeamType.INDUSTRY).withOrgGroupId(5000).build();
    when(teamRepository.findAllTeamsThatUserIsMemberOf(user.wuaId())).thenReturn(List.of(team));
    assertTrue(teamService.userIsMemberOfOrganisationGroupTeam(team.getEnergyPortalOrgGroupId(), user));
  }

  @Test
  void userIsMemberOfRegulatorTeam_IsMember() {
    var user = ServiceUserDetailTestUtil.Builder().build();
    var team = TeamTestUtil.Builder().withTeamType(TeamType.REGULATOR).build();
    when(teamRepository.findAllTeamsThatUserIsMemberOf(user.wuaId())).thenReturn(List.of(team));
    assertTrue(teamService.userIsMemberOfRegulatorTeam(user));
  }

  @Test
  void userIsMemberOfRegulatorTeam_NotMember() {
    var user = ServiceUserDetailTestUtil.Builder().build();
    var team = TeamTestUtil.Builder().withTeamType(TeamType.INDUSTRY).build();
    when(teamRepository.findAllTeamsThatUserIsMemberOf(user.wuaId())).thenReturn(List.of(team));
    assertFalse(teamService.userIsMemberOfRegulatorTeam(user));
  }
}