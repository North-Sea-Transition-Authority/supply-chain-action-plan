package uk.co.nstauthority.scap.permissionmanagement.teams;

import static java.util.Collections.emptyList;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrowsExactly;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import java.util.Collections;
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
import uk.co.nstauthority.scap.energyportal.WebUserAccountId;
import uk.co.nstauthority.scap.error.exception.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.permissionmanagement.Team;
import uk.co.nstauthority.scap.permissionmanagement.TeamId;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberTestUtil;
import uk.co.nstauthority.scap.permissionmanagement.TeamRepository;
import uk.co.nstauthority.scap.permissionmanagement.TeamTestUtil;
import uk.co.nstauthority.scap.permissionmanagement.TeamType;
import uk.co.nstauthority.scap.permissionmanagement.industry.IndustryTeamRole;
import uk.co.nstauthority.scap.permissionmanagement.regulator.RegulatorTeamRole;

@ExtendWith(MockitoExtension.class)
class TeamServiceTest {

  private static final Long WUA_ID = ServiceUserDetailTestUtil.Builder()
      .build()
      .wuaId();

  @Mock
  private TeamRepository teamRepository;

  @Mock
  private TeamMemberRoleService teamMemberRoleService;

  @Mock
  private TeamMemberService teamMemberService;

  @InjectMocks
  private TeamService teamService;

  @Test
  void getAllTeams_VerifyCalls() {
    teamService.getAllTeams();
    verify(teamRepository).findAll();
  }

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

  @Test
  void findTeamsByUser_accessManager_getsAllTeams() {
    var user = ServiceUserDetailTestUtil.Builder().build();
    var regulatorTeam = TeamTestUtil
        .Builder()
        .withTeamType(TeamType.REGULATOR)
        .build();

    when(teamRepository.getTeamByTeamType(TeamType.REGULATOR)).thenReturn(regulatorTeam);
    when(teamMemberService.isMemberOfTeamWithAnyRoleOf(
        new TeamId(regulatorTeam.getUuid()),
        user,
        Collections.singleton(RegulatorTeamRole.ORGANISATION_ACCESS_MANAGER.name())))
        .thenReturn(true);

    teamService.findTeamsByUser(user);
    verify(teamRepository).findAll();
  }

  @Test
  void findTeamsByUser_notAccessManager_getsTeamsUserBelongsTo() {
    var user = ServiceUserDetailTestUtil.Builder().build();
    var regulatorTeam = TeamTestUtil
        .Builder()
        .withTeamType(TeamType.REGULATOR)
        .build();

    when(teamRepository.getTeamByTeamType(TeamType.REGULATOR)).thenReturn(regulatorTeam);
    when(teamMemberService.isMemberOfTeamWithAnyRoleOf(
        new TeamId(regulatorTeam.getUuid()),
        user,
        Collections.singleton(RegulatorTeamRole.ORGANISATION_ACCESS_MANAGER.name())))
        .thenReturn(false);

    teamService.findTeamsByUser(user);
    verify(teamRepository).findAllTeamsThatUserIsMemberOf(user.wuaId());
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
  void getTeamByOrgGroupId_NoMatch_Throws() {
    var team = TeamTestUtil
        .Builder()
        .withTeamType(TeamType.INDUSTRY)
        .build();
    assertThrowsExactly(ScapEntityNotFoundException.class, () -> teamService.getByEnergyPortalOrgGroupId(team.getEnergyPortalOrgGroupId()));
  }

  @Test
  void getTeamByOrgGroupId_Match_TeamReturend() {
    var team = TeamTestUtil
        .Builder()
        .withTeamType(TeamType.INDUSTRY)
        .build();

    when(teamRepository.findByEnergyPortalOrgGroupId(team.getEnergyPortalOrgGroupId())).thenReturn(Optional.of(team));
    var result = teamService.getByEnergyPortalOrgGroupId(team.getEnergyPortalOrgGroupId());

    assertThat(result).isEqualTo(team);
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
  void userIsMemberofOrganisationGroupTeam_RegulatorAndOrgGroup() {
    var user = ServiceUserDetailTestUtil.Builder().build();
    var regulatorTeam = TeamTestUtil.Builder().withTeamType(TeamType.REGULATOR).withOrgGroupId(null).build();
    var industryTeam = TeamTestUtil.Builder().withTeamType(TeamType.INDUSTRY).withOrgGroupId(116).build();
    when(teamRepository.findAllTeamsThatUserIsMemberOf(user.wuaId())).thenReturn(List.of(regulatorTeam, industryTeam));
    assertTrue(teamService.userIsMemberOfOrganisationGroupTeam(industryTeam.getEnergyPortalOrgGroupId(), user));
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

  @Test
  void archiveTeam_VerifyCalls() {
    var team = TeamTestUtil.Builder().withTeamType(TeamType.INDUSTRY).build();
    teamService.archiveTeam(team);

    var inOrder = inOrder(teamMemberRoleService, teamRepository);
    inOrder.verify(teamMemberRoleService).deleteUsersInTeam(team);
    inOrder.verify(teamRepository).delete(team);
  }

  @Test
  void getRegulatorTeam() {
    var regTeam = new Team();

    when(teamRepository.getTeamByTeamType(TeamType.REGULATOR)).thenReturn(regTeam);

    var returnedTeam = teamService.getRegulatorTeam();

    assertThat(returnedTeam).isEqualTo(regTeam);
  }

  @Test
  void getPermissionsForUserInOrganisationGroup_whenUserIsInTeam_AndHasNoRoles_thenEmptyList() {
    var team = TeamTestUtil.Builder()
        .withOrgGroupId(1)
        .withTeamType(TeamType.INDUSTRY)
        .build();

    when(teamRepository.findByEnergyPortalOrgGroupId(team.getEnergyPortalOrgGroupId())).thenReturn(Optional.of(team));
    when(teamMemberService.findTeamMember(team, new WebUserAccountId(WUA_ID)))
        .thenReturn(Optional.empty());

    var resultingPermissions = teamService.findAllPermissionsForUserInOrganisationGroup(WUA_ID, team.getEnergyPortalOrgGroupId());
    assertThat(resultingPermissions).isEmpty();
  }

  @Test
  void getPermissionsForUserInOrganisationGroup_whenUserIsInNoTeam_thenEmptyList() {
    var orgId = 1;
    when(teamRepository.findByEnergyPortalOrgGroupId(orgId)).thenReturn(Optional.empty());

    var resultingPermissions = teamService.findAllPermissionsForUserInOrganisationGroup(WUA_ID, orgId);
    assertThat(resultingPermissions).isEmpty();
    verify(teamMemberService, never()).findTeamMember(any(), any());
  }

  @Test
  void getPermissionsForUserInOrganisationGroup_whenUserIsInTeam_AndHasRoles_thenPopulatedList() {
    var team = TeamTestUtil.Builder()
        .withOrgGroupId(1)
        .withTeamType(TeamType.INDUSTRY)
        .build();
    var teamMember = TeamMemberTestUtil.Builder()
        .withTeamType(TeamType.INDUSTRY)
        .withRole(IndustryTeamRole.SCAP_SUBMITTER)
        .build();

    when(teamRepository.findByEnergyPortalOrgGroupId(team.getEnergyPortalOrgGroupId())).thenReturn(Optional.of(team));
    when(teamMemberService.findTeamMember(team, new WebUserAccountId(WUA_ID)))
        .thenReturn(Optional.of(teamMember));

    var resultingPermissions = teamService.findAllPermissionsForUserInOrganisationGroup(WUA_ID, team.getEnergyPortalOrgGroupId());
    assertThat(resultingPermissions)
        .containsExactly(IndustryTeamRole.SCAP_SUBMITTER.getRolePermissions().toArray(RolePermission[]::new));
  }

  @Test
  void getTeamsForUserBasedOnPermission_whenUserIsNotInTeam_thenEmptyList() {
    when(teamRepository.findAllTeamsThatUserIsMemberOf(WUA_ID)).thenReturn(List.of());
    var resultingTeams =  teamService.findAllTeamsForUserBasedOnPermission(List.of(IndustryTeamRole.SCAP_SUBMITTER), WUA_ID);
    assertThat(resultingTeams).isEmpty();
  }

  @Test
  void getPermissionsForUserInOrganisationGroup_whenNoPermissionsMatch_thenEmptyList() {
    var team = TeamTestUtil.Builder()
        .withTeamType(TeamType.INDUSTRY)
        .build();

    var teamMemberRole = TeamMemberRoleTestUtil.Builder()
        .withRole("SCAP_VIEWER")
        .withTeam(team)
        .build();

    when(teamRepository.findAllTeamsThatUserIsMemberOf(WUA_ID)).thenReturn(List.of(team));
    when(teamMemberService.findAllRolesByUser(List.of(team), new WebUserAccountId(WUA_ID)))
        .thenReturn(List.of(teamMemberRole));

    var resultingTeams =  teamService.findAllTeamsForUserBasedOnPermission(List.of(IndustryTeamRole.SCAP_SUBMITTER), WUA_ID);
    assertThat(resultingTeams).isEmpty();
  }

  @Test
  void getTeamsForUserBasedOnPermission_whenPermissionMatches_thenPopulatedList() {
    var team = TeamTestUtil.Builder()
        .withTeamType(TeamType.INDUSTRY)
        .build();

    var teamMemberRole = TeamMemberRoleTestUtil.Builder()
        .withRole("SCAP_SUBMITTER")
        .withTeam(team)
        .build();

    when(teamRepository.findAllTeamsThatUserIsMemberOf(WUA_ID)).thenReturn(List.of(team));
    when(teamMemberService.findAllRolesByUser(List.of(team), new WebUserAccountId(WUA_ID)))
        .thenReturn(List.of(teamMemberRole));

    var resultingTeams =  teamService.findAllTeamsForUserBasedOnPermission(List.of(IndustryTeamRole.SCAP_SUBMITTER), WUA_ID);
    assertThat(resultingTeams).containsExactly(team);
  }

  @Test
  void getTeamsForUserBasedOnPermission_whenUserIsMemberOfMultipleTeams_onlyReturnTeamWithCorrectPermission() {
    var industryTeam = TeamTestUtil.Builder()
        .withTeamType(TeamType.INDUSTRY)
        .build();

    var industryTeamMemberRole = TeamMemberRoleTestUtil.Builder()
        .withRole("SCAP_SUBMITTER")
        .withTeam(industryTeam)
        .build();

    var regulatorTeam = TeamTestUtil.Builder()
        .withTeamType(TeamType.REGULATOR)
        .build();

    var regulatorTeamMemberRole = TeamMemberRoleTestUtil.Builder()
        .withRole("SCAP_CASE_OFFICER")
        .withTeam(regulatorTeam)
        .build();

    when(teamRepository.findAllTeamsThatUserIsMemberOf(WUA_ID)).thenReturn(List.of(industryTeam, regulatorTeam));
    when(teamMemberService.findAllRolesByUser(List.of(industryTeam, regulatorTeam), new WebUserAccountId(WUA_ID)))
        .thenReturn(List.of(regulatorTeamMemberRole, industryTeamMemberRole));

    var resultingTeams =  teamService.findAllTeamsForUserBasedOnPermission(
        List.of(IndustryTeamRole.SCAP_SUBMITTER),
        WUA_ID
    );
    assertThat(resultingTeams).containsExactly(industryTeam);
  }
}
