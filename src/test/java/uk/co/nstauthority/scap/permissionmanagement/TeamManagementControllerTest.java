package uk.co.nstauthority.scap.permissionmanagement;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.csrf;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.model;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.redirectedUrl;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.view;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;
import static uk.co.nstauthority.scap.authentication.TestUserProvider.user;
import static uk.co.nstauthority.scap.mvc.ReverseRouter.emptyBindingResult;
import static uk.co.nstauthority.scap.utils.ControllerTestingUtil.redirectUrl;

import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.ContextConfiguration;
import uk.co.fivium.energyportalapi.generated.types.OrganisationGroup;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.industry.AbstractIndustryTeamControllerTest;
import uk.co.nstauthority.scap.permissionmanagement.regulator.RegulatorTeamRole;
import uk.co.nstauthority.scap.permissionmanagement.teams.NewTeamForm;
import uk.co.nstauthority.scap.permissionmanagement.teams.NewTeamFormvalidator;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamManagementController;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamView;
import uk.co.nstauthority.scap.scap.organisationgroup.OrganisationGroupService;

@ContextConfiguration(classes = TeamManagementController.class)
class TeamManagementControllerTest extends AbstractIndustryTeamControllerTest {

  @MockBean
  private OrganisationGroupService organisationGroupService;

  @MockBean
  private NewTeamFormvalidator newTeamFormvalidator;

  @Test
  void renderTeamList_notAuthorised_thenUnAuthorised() throws Exception {
    mockMvc.perform(
        get(ReverseRouter.route(on(TeamManagementController.class).renderTeamList())))
        .andExpect(status().is3xxRedirection());
  }

  @Test
  void renderTeamList_authorised_thenOk() throws Exception {
    mockMvc.perform(
            get(ReverseRouter.route(on(TeamManagementController.class).renderTeamList()))
              .with(user(testUser)))
        .andExpect(status().isOk());
  }

  @Test
  void renderTeamList_userInTeam_thenTeamPresent() throws Exception {
    var team1 = new TeamView(new TeamId(UUID.randomUUID()), TeamType.INDUSTRY, "CENTRICA");
    var team2 = new TeamView(new TeamId(UUID.randomUUID()), TeamType.INDUSTRY, "SHELL");

    when(userDetailService.getUserDetail()).thenReturn(testUser);
    when(teamService.findTeamsByUser(testUser)).thenReturn(List.of(team1, team2));

    mockMvc.perform(
        get(ReverseRouter.route(on(TeamManagementController.class).renderTeamList()))
            .with(user(testUser)))
        .andExpect(status().isOk())
        .andExpect(model().attribute("allTeams", List.of(team1, team2)))
        .andExpect(model().attribute("hasCreateTeamPermissions", false));
  }

  @Test
  void renderTeamList_userIn1Team_thenRedirect() throws Exception {
    var team1 = new TeamView(new TeamId(UUID.randomUUID()), TeamType.INDUSTRY, "CENTRICA");

    when(userDetailService.getUserDetail()).thenReturn(testUser);
    when(teamService.findTeamsByUser(testUser)).thenReturn(List.of(team1));

    mockMvc.perform(
        get(ReverseRouter.route(on(TeamManagementController.class).renderTeamList()))
            .with(user(testUser)))
        .andExpect(status().is3xxRedirection())
        .andExpect(redirectedUrl("/permission-management/industry/%s".formatted(team1.teamId().uuid())));
  }

  @Test
  void renderTeamList_userAccessManager_thenAllTeamPresent() throws Exception {
    var team = TeamTestUtil
        .Builder()
        .withTeamType(TeamType.INDUSTRY)
        .build();

    var regulatorTeam = TeamTestUtil
        .Builder()
        .withTeamType(TeamType.REGULATOR)
        .build();

    var roles = Set.of(RegulatorTeamRole.ORGANISATION_ACCESS_MANAGER.name());

    when(userDetailService.getUserDetail()).thenReturn(testUser);
    when(teamService.getTeamsOfTypeThatUserBelongsTo(testUser, TeamType.REGULATOR)).thenReturn(List.of(regulatorTeam));
    when(teamMemberService.getTeamMember(any(Team.class), eq(testUser.getWebUserAccountId()))).thenReturn(
        TeamMemberTestUtil.Builder()
            .withRole(RegulatorTeamRole.ORGANISATION_ACCESS_MANAGER)
            .build());
    when(teamService.findTeamsByUser(testUser)).thenReturn(List.of(TeamView.fromTeam(team), TeamView.fromTeam(regulatorTeam)));
    when(teamService.getRegulatorTeam()).thenReturn(regulatorTeam);
    when(teamMemberService.isMemberOfTeamWithAnyRoleOf(eq(TeamId.valueOf(regulatorTeam.getUuid())), eq(testUser), eq(roles))).thenReturn(true);
    mockMvc.perform(
            get(ReverseRouter.route(on(TeamManagementController.class).renderTeamList()))
                .with(user(testUser)))
        .andExpect(status().isOk())
        .andExpect(model().attribute("allTeams", List.of(TeamView.fromTeam(team), TeamView.fromTeam(regulatorTeam))))
        .andExpect(model().attribute("hasCreateTeamPermissions", true));
  }

  @Test
  void renderTeamList_notUserAccessManager_thenRedirect() throws Exception {
    var regulatorTeam = TeamTestUtil
        .Builder()
        .withTeamType(TeamType.REGULATOR)
        .build();

    when(userDetailService.getUserDetail()).thenReturn(testUser);
    when(teamService.getTeamsOfTypeThatUserBelongsTo(testUser, TeamType.REGULATOR)).thenReturn(List.of(regulatorTeam));
    when(teamService.findTeamsByUser(testUser)).thenReturn(List.of(TeamView.fromTeam(regulatorTeam)));
    when(teamMemberService.getTeamMember(any(Team.class), eq(testUser.getWebUserAccountId()))).thenReturn(
        TeamMemberTestUtil.Builder()
            .withRole(RegulatorTeamRole.SCAP_VIEWER)
            .build());
    when(teamService.getTeamsThatUserBelongsTo(testUser)).thenReturn(List.of(regulatorTeam));
    mockMvc.perform(
            get(ReverseRouter.route(on(TeamManagementController.class).renderTeamList()))
                .with(user(testUser)))
        .andExpect(status().is3xxRedirection())
        .andExpect(redirectedUrl("/permission-management/regulator/%s".formatted(regulatorTeam.getUuid())));
  }

  @Test
  void renderNewTeamForm_authorisedUser_isOk() throws Exception {

    mockMvc.perform(
        get(ReverseRouter.route(on(TeamManagementController.class).renderNewIndustryTeamForm(new NewTeamForm())))
            .with(user(testUser)))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/permissionmanagement/addTeam"))
    ;
  }

  @Test
  void addNewIndustryTeam_FormValidates_ReturnToTeamList() throws Exception {

    var orgGroup = new OrganisationGroup();
    orgGroup.setName("Royal Dutch Shell");
    orgGroup.setOrganisationGroupId(10000);

    var form = new NewTeamForm();
    form.setOrganisationGroupId("10000");

    when(teamService.validate(any(), any())).thenReturn(emptyBindingResult());
    when(organisationGroupService.getOrganisationGroupById(any(), any())).thenReturn(Optional.of(orgGroup));when(userDetailService.getUserDetail()).thenReturn(testUser);
    when(userDetailService.getUserDetail()).thenReturn(testUser);

    mockMvc.perform(
            post(ReverseRouter.route(on(TeamManagementController.class).addNewIndustryTeam(form, emptyBindingResult())))
                .flashAttr("form", form)
                .with(csrf())
                .with(user(testUser)))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/permissionmanagement/teamList"));
  }

  @Test
  void addNewIndustryTeam_FormValidatesGroupDoesntExist_ReturnToNewTeam() throws Exception {

    var form = new NewTeamForm();
    form.setOrganisationGroupId("10000");

    when(teamService.validate(any(), any())).thenReturn(emptyBindingResult());
    when(userDetailService.getUserDetail()).thenReturn(testUser);

    mockMvc.perform(
            post(ReverseRouter.route(on(TeamManagementController.class).addNewIndustryTeam(form, emptyBindingResult())))
                .flashAttr("form", form)
                .with(csrf())
                .with(user(testUser)))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/permissionmanagement/addTeam"));
  }

  @Test
  void renderArchiveTeamConfirmation_userNotPermitted() throws Exception {
    var teamId = new TeamId(new UUID(1, 1));

    when(teamMemberService.getAllPermissionsForUser(testUser)).thenReturn(List.of(RolePermission.GRANT_ROLES));
    doReturn(testUser).when(userDetailService).getUserDetail();

    mockMvc.perform(
        get(ReverseRouter.route(on(TeamManagementController.class).renderArchiveTeamConfirmation(teamId)))
            .with(csrf())
            .with(user(testUser)))
        .andExpect(status().is4xxClientError());
  }

  @Test
  void archiveTeamConfirmation_userPermitted() throws Exception {
    var teamId = new TeamId(new UUID(1, 1));

    mockMvc.perform(
        get(ReverseRouter.route(on(TeamManagementController.class).renderArchiveTeamConfirmation(teamId)))
            .with(csrf())
            .with(user(testUser)))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/permissionmanagement/removeTeam"));
  }

  @Test
  void renderArchiveTeamConfirmation_userNotPermittedReroute() throws Exception {
    var teamId = new TeamId(new UUID(1, 1));

    when(teamMemberService.getAllPermissionsForUser(testUser)).thenReturn(List.of(RolePermission.GRANT_ROLES));
    doReturn(testUser).when(userDetailService).getUserDetail();

    mockMvc.perform(
            get(ReverseRouter.route(on(TeamManagementController.class).renderArchiveTeamConfirmation(teamId)))
                .with(csrf())
                .with(user(testUser)))
        .andExpect(status().is4xxClientError());
  }

  @Test
  void archiveTeam_VerifyCalls() throws Exception {
    var teamId = new TeamId(new UUID(1, 1));
    var team = TeamTestUtil
        .Builder()
        .withId(teamId.uuid())
        .build();
    when(teamService.getTeam(teamId)).thenReturn(team);

    mockMvc.perform(
            post(ReverseRouter.route(on(TeamManagementController.class).archiveTeam(teamId, null)))
                .with(csrf())
                .with(user(testUser)))
        .andExpect(status().is3xxRedirection())
        .andExpect(redirectUrl("/permission-management/"));

    verify(teamService).archiveTeam(team);
  }
}
