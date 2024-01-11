package uk.co.nstauthority.scap.permissionmanagement.teams;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.tuple;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.List;
import java.util.Set;
import java.util.UUID;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.authentication.ServiceUserDetailTestUtil;
import uk.co.nstauthority.scap.energyportal.WebUserAccountId;
import uk.co.nstauthority.scap.error.exception.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.permissionmanagement.Team;
import uk.co.nstauthority.scap.permissionmanagement.TeamId;
import uk.co.nstauthority.scap.permissionmanagement.TeamMember;
import uk.co.nstauthority.scap.permissionmanagement.TeamTestUtil;
import uk.co.nstauthority.scap.permissionmanagement.TeamType;
import uk.co.nstauthority.scap.permissionmanagement.industry.IndustryTeamRole;
import uk.co.nstauthority.scap.permissionmanagement.regulator.RegulatorTeamRole;

@ExtendWith(MockitoExtension.class)
class TeamMemberServiceTest {

  @Mock
  private TeamMemberRoleRepository teamMemberRoleRepository;

  @InjectMocks
  private TeamMemberService teamMemberService;

  @Test
  void getAllTeamMembers_whenMembers_thenNotEmpty() {
    var team = new Team();
    team.setTeamType(TeamType.REGULATOR);

    var roleBuilder = TeamMemberRoleTestUtil.Builder()
        .withTeam(team)
        .withWebUserAccountId(1L);

    var teamMemberRoleAccessManager = roleBuilder.withRole(RegulatorTeamRole.ACCESS_MANAGER.name())
        .build();

    var teamMemberRoleOrgAccessManager = roleBuilder.withRole(RegulatorTeamRole.ORGANISATION_ACCESS_MANAGER.name())
        .build();

    when(teamMemberRoleRepository.findAllByTeam(team)).thenReturn(
        List.of(teamMemberRoleAccessManager, teamMemberRoleOrgAccessManager));

    var result = teamMemberService.getTeamMembers(team);

    var teamView = TeamView.fromTeam(team);

    var expectedTeamMember = new TeamMember(new WebUserAccountId(1L), teamView, Set.of(RegulatorTeamRole.ACCESS_MANAGER,
        RegulatorTeamRole.ORGANISATION_ACCESS_MANAGER));

    assertThat(result).containsExactly(expectedTeamMember);
  }

  @Test
  void getAllTeamMembers_whenNoMembers_thenEmpty() {
    var team = new Team();

    when(teamMemberRoleRepository.findAllByTeam(team)).thenReturn(List.of());

    var result = teamMemberService.getTeamMembers(team);

    assertThat(result).isEmpty();
    verify(teamMemberRoleRepository).findAllByTeam(team);
  }

  @Test
  void isMemberOfTeam_whenMember_thenTrue() {
    var user = ServiceUserDetailTestUtil.Builder().build();
    var teamId = new TeamId(UUID.randomUUID());

    when(teamMemberRoleRepository.existsByWuaIdAndTeamUuid(user.wuaId(), teamId.uuid())).thenReturn(true);

    assertTrue(teamMemberService.isMemberOfTeam(teamId, user));
  }

  @Test
  void isMemberOfTeam_whenNotMember_thenFalse() {
    var user = ServiceUserDetailTestUtil.Builder().build();
    var teamId = new TeamId(UUID.randomUUID());

    when(teamMemberRoleRepository.existsByWuaIdAndTeamUuid(user.wuaId(), teamId.uuid())).thenReturn(false);

    assertFalse(teamMemberService.isMemberOfTeam(teamId, user));
  }

  @Test
  void isMemberOfTeamWithAnyRoleOf_whenMemberWithRole_thenTrue() {
    var user = ServiceUserDetailTestUtil.Builder().build();
    var teamId = new TeamId(UUID.randomUUID());
    var roles = Set.of("ROLE_NAME");

    when(teamMemberRoleRepository.existsByWuaIdAndTeamUuidAndRoleIn(user.wuaId(), teamId.uuid(), roles))
        .thenReturn(true);

    assertTrue(teamMemberService.isMemberOfTeamWithAnyRoleOf(teamId, user, roles));
  }

  @Test
  void isMemberOfTeamWithAnyRoleOf_whenNotMemberWithRole_thenFalse() {
    var user = ServiceUserDetailTestUtil.Builder().build();
    var teamId = new TeamId(UUID.randomUUID());
    var roles = Set.of("ROLE_NAME");

    when(teamMemberRoleRepository.existsByWuaIdAndTeamUuidAndRoleIn(user.wuaId(), teamId.uuid(), roles))
        .thenReturn(false);

    var result = teamMemberService.isMemberOfTeamWithAnyRoleOf(teamId, user, roles);

    assertFalse(teamMemberService.isMemberOfTeamWithAnyRoleOf(teamId, user, roles));
  }

  @Test
  void getTeamMemberOrThrow_noTeamMemberThrows() {
    var team = TeamTestUtil.Builder().build();
    var webUserAccountId = new WebUserAccountId(100L);
    assertThrows(ScapEntityNotFoundException.class,
        () -> teamMemberService.getTeamMember(team, webUserAccountId));
  }

  @Test
  void getTeamMembers_whenTeamMemberHasMultipleRoles_thenRolesMappedCorrectly() {

    var team = TeamTestUtil.Builder().build();

    var webUserAccountId = new WebUserAccountId(100L);

    var firstRole = RegulatorTeamRole.ACCESS_MANAGER;
    var secondRole = RegulatorTeamRole.ORGANISATION_ACCESS_MANAGER;

    team.setTeamType(TeamType.REGULATOR);

    var roleBuilder = TeamMemberRoleTestUtil.Builder()
        .withTeam(team)
        .withWebUserAccountId(webUserAccountId.id());

    // GIVEN a user with multiple roles in the same team
    var firstTeamMemberRole = roleBuilder
        .withRole(firstRole.name())
        .build();

    var secondTeamMemberRole = roleBuilder
        .withRole(secondRole.name())
        .build();

    when(teamMemberRoleRepository.findAllByTeam(team)).thenReturn(
        List.of(firstTeamMemberRole, secondTeamMemberRole));

    // WHEN we get the team members for that team
    var resultingTeamMembers = teamMemberService.getTeamMembers(team);

    // THEN one team member is returned with the multiple roles
    assertThat(resultingTeamMembers)
        .extracting(TeamMember::wuaId, TeamMember::roles)
        .containsExactly(
            tuple(
                webUserAccountId,
                Set.of(secondRole, firstRole)
            )
        );
  }

  @Test
  void getTeamMembers_whenIndustryTeamMemberHasMultipleRoles_thenRolesMappedCorrectly() {

    var team = TeamTestUtil.Builder().build();

    var webUserAccountId = new WebUserAccountId(100L);

    var firstRole = IndustryTeamRole.ACCESS_MANAGER;
    var secondRole = IndustryTeamRole.SCAP_SUBMITTER;

    team.setTeamType(TeamType.INDUSTRY);

    var roleBuilder = TeamMemberRoleTestUtil.Builder()
        .withTeam(team)
        .withWebUserAccountId(webUserAccountId.id());

    // GIVEN a user with multiple roles in the same team
    var firstTeamMemberRole = roleBuilder
        .withRole(firstRole.name())
        .build();

    var secondTeamMemberRole = roleBuilder
        .withRole(secondRole.name())
        .build();

    when(teamMemberRoleRepository.findAllByTeam(team)).thenReturn(
        List.of(firstTeamMemberRole, secondTeamMemberRole));

    // WHEN we get the team members for that team
    var resultingTeamMembers = teamMemberService.getTeamMembers(team);

    // THEN one team member is returned with the multiple roles
    assertThat(resultingTeamMembers)
        .extracting(TeamMember::wuaId, TeamMember::roles)
        .containsExactly(
            tuple(
                webUserAccountId,
                Set.of(secondRole, firstRole)
            )
        );
  }

  @Test
  void findTeamMember_whenMemberIsInTeam_thenGetTeamMember() {
    var team = new Team(UUID.randomUUID());
    team.setTeamType(TeamType.REGULATOR);

    var wuaId = new WebUserAccountId(123L);
    var role = TeamMemberRoleTestUtil.Builder()
        .withRole(RegulatorTeamRole.ORGANISATION_ACCESS_MANAGER.name())
        .withWebUserAccountId(wuaId.id())
        .build();

    var teamView = TeamView.fromTeam(team);

    when(teamMemberRoleRepository.findAllByTeamAndWuaId(team, wuaId.id()))
        .thenReturn(List.of(role));

    var result = teamMemberService.findTeamMember(team, wuaId);

    assertTrue(result.isPresent());
    assertThat(result.get()).extracting(
        TeamMember::teamView,
        TeamMember::wuaId
    ).containsExactly(
        teamView,
        wuaId
    );

    assertThat(result.get().roles()).containsExactly(
        RegulatorTeamRole.ORGANISATION_ACCESS_MANAGER
    );
  }

  @Test
  void findTeamMember_whenMemberIsNotInTeam_thenEmpty() {
    var team = new Team(UUID.randomUUID());
    team.setTeamType(TeamType.REGULATOR);

    var wuaId = new WebUserAccountId(123L);

    when(teamMemberRoleRepository.findAllByTeamAndWuaId(team, wuaId.id())).thenReturn(List.of());

    var result = teamMemberService.findTeamMember(team, wuaId);

    assertTrue(result.isEmpty());
  }

  @Test
  void findTeamMemberRole_whenMemberIsInTeam_thenReturn() {
    var team = new Team(UUID.randomUUID());
    team.setTeamType(TeamType.REGULATOR);

    var wuaId = new WebUserAccountId(123L);
    var role = TeamMemberRoleTestUtil.Builder()
        .withRole(RegulatorTeamRole.ORGANISATION_ACCESS_MANAGER.name())
        .withWebUserAccountId(wuaId.id())
        .build();

    when(teamMemberRoleRepository.findAllByTeamInAndWuaId(List.of(team), wuaId.id()))
        .thenReturn(List.of(role));
    var result = teamMemberService.findAllRolesByUser(List.of(team), wuaId);

    assertThat(result)
        .extracting(
            TeamMemberRole::getRole,
            teamMemberRole -> teamMemberRole.getTeam().getTeamType(),
            TeamMemberRole::getWuaId
    ).containsExactly(
        tuple(role.getRole(), team.getTeamType(), wuaId.id())
    );
  }

  @Test
  void findTeamMemberRole_whenMemberIsNotInTeam_thenEmptyList() {
    var team = new Team(UUID.randomUUID());
    team.setTeamType(TeamType.INDUSTRY);

    var wuaId = new WebUserAccountId(123L);

    when(teamMemberRoleRepository.findAllByTeamInAndWuaId(List.of(team), wuaId.id())).thenReturn(List.of());

    var result = teamMemberService.findAllRolesByUser(List.of(team), wuaId);
    assertTrue(result.isEmpty());
  }

  @Test
  void findAllPermissionsForUser_HasNoRoles() {
    var user = ServiceUserDetailTestUtil.Builder().build();

    var roles = teamMemberService.getAllPermissionsForUser(user);
    assertThat(roles).isEmpty();
  }

  @Test
  void findAllPermissionsByUser_UserHasRoles() {
    var user = ServiceUserDetailTestUtil.Builder().build();
    var industryTeam = TeamTestUtil.Builder().withTeamType(TeamType.INDUSTRY).build();
    var industryRole = TeamMemberRoleTestUtil
        .Builder()
        .withRole(IndustryTeamRole.SCAP_VIEWER.name())
        .withTeam(industryTeam)
        .build();
    var regulatorRole = TeamMemberRoleTestUtil
        .Builder()
        .withRole(RegulatorTeamRole.SCAP_CASE_OFFICER.name())
        .build();
    when(teamMemberRoleRepository.findAllByWuaId(user.wuaId())).thenReturn(List.of(industryRole, regulatorRole));

    var roles = teamMemberService.getAllPermissionsForUser(user);
    assertThat(roles).contains(RolePermission.VIEW_SCAP, RolePermission.REVIEW_SCAP);
  }

  @Test
  void findAllPermissionsByUser_UserHasIndustryRoles() {
    var industryTeam = TeamTestUtil.Builder().withTeamType(TeamType.INDUSTRY).build();
    var user = ServiceUserDetailTestUtil.Builder().build();
    var industryRole = TeamMemberRoleTestUtil
        .Builder()
        .withRole(IndustryTeamRole.SCAP_VIEWER.name())
        .withTeam(industryTeam)
        .build();
    var industryRole2 = TeamMemberRoleTestUtil
        .Builder()
        .withRole(IndustryTeamRole.SCAP_SUBMITTER.name())
        .withTeam(industryTeam)
        .build();
    when(teamMemberRoleRepository.findAllByWuaId(user.wuaId())).thenReturn(List.of(industryRole, industryRole2));

    var roles = teamMemberService.getAllPermissionsForUser(user);
    assertThat(roles).contains(RolePermission.VIEW_SCAP, RolePermission.SUBMIT_SCAP);
  }

  @Test
  void findAllPermissionsByUser_UserHasRegulatorRoles() {
    var user = ServiceUserDetailTestUtil.Builder().build();
    var regulatorRole1 = TeamMemberRoleTestUtil
        .Builder()
        .withRole(RegulatorTeamRole.SCAP_VIEWER.name())
        .build();
    var regulatorRole2 = TeamMemberRoleTestUtil
        .Builder()
        .withRole(RegulatorTeamRole.SCAP_CASE_OFFICER.name())
        .build();
    when(teamMemberRoleRepository.findAllByWuaId(user.wuaId())).thenReturn(List.of(regulatorRole1, regulatorRole2));

    var roles = teamMemberService.getAllPermissionsForUser(user);
    assertThat(roles).contains(RolePermission.VIEW_SCAP, RolePermission.REVIEW_SCAP);
  }
}