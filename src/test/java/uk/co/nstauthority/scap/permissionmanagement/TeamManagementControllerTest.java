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
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.view;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;
import static uk.co.nstauthority.scap.authentication.TestUserProvider.user;
import static uk.co.nstauthority.scap.mvc.ReverseRouter.emptyBindingResult;
import static uk.co.nstauthority.scap.utils.ControllerTestingUtil.redirectUrl;

import java.util.List;
import java.util.Optional;
import java.util.UUID;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.ContextConfiguration;
import uk.co.fivium.energyportalapi.generated.types.OrganisationGroup;
import uk.co.nstauthority.scap.authentication.ServiceUserDetailTestUtil;
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
        .andExpect(status().isUnauthorized());
  }

  @Test
  void renderTeamList_authorised_thenOk() throws Exception {
    var user = ServiceUserDetailTestUtil.Builder().build();
    mockMvc.perform(
            get(ReverseRouter.route(on(TeamManagementController.class).renderTeamList()))
              .with(user(user)))
        .andExpect(status().isOk());
  }

  @Test
  void renderTeamList_userInTeam_thenTeamPresent() throws Exception {
    var user = ServiceUserDetailTestUtil.Builder().build();
    var team = TeamTestUtil
        .Builder()
        .withTeamType(TeamType.INDUSTRY)
        .build();

    when(userDetailService.getUserDetail()).thenReturn(user);
    when(teamService.getTeamsThatUserBelongsTo(user)).thenReturn(List.of(team));

    mockMvc.perform(
        get(ReverseRouter.route(on(TeamManagementController.class).renderTeamList()))
            .with(user(user)))
        .andExpect(status().isOk())
        .andExpect(model().attribute("allTeams", List.of(TeamView.fromTeam(team))));
  }

  @Test
  void renderTeamList_userAccessManager_thenAllTeamPresent() throws Exception {
    var user = ServiceUserDetailTestUtil.Builder().build();
    var team = TeamTestUtil
        .Builder()
        .withTeamType(TeamType.INDUSTRY)
        .build();

    var regulatorTeam = TeamTestUtil
        .Builder()
        .withTeamType(TeamType.REGULATOR)
        .build();

    when(userDetailService.getUserDetail()).thenReturn(user);
    when(teamService.getTeamsOfTypeThatUserBelongsTo(user, TeamType.REGULATOR)).thenReturn(List.of(regulatorTeam));
    when(teamMemberService.getTeamMember(any(Team.class), eq(user.getWebUserAccountId()))).thenReturn(
        TeamMemberTestUtil.Builder()
            .withRole(RegulatorTeamRole.ORGANISATION_ACCESS_MANAGER)
            .build());
    when(teamService.getAllTeams()).thenReturn(List.of(team, regulatorTeam));
    mockMvc.perform(
            get(ReverseRouter.route(on(TeamManagementController.class).renderTeamList()))
                .with(user(user)))
        .andExpect(status().isOk())
        .andExpect(model().attribute("allTeams", List.of(TeamView.fromTeam(team), TeamView.fromTeam(regulatorTeam))));
  }

  @Test
  void renderTeamList_notUserAccessManager_thenRegulatorTeamPresent() throws Exception {
    var user = ServiceUserDetailTestUtil.Builder().build();
    var regulatorTeam = TeamTestUtil
        .Builder()
        .withTeamType(TeamType.REGULATOR)
        .build();

    when(userDetailService.getUserDetail()).thenReturn(user);
    when(teamService.getTeamsOfTypeThatUserBelongsTo(user, TeamType.REGULATOR)).thenReturn(List.of(regulatorTeam));
    when(teamMemberService.getTeamMember(any(Team.class), eq(user.getWebUserAccountId()))).thenReturn(
        TeamMemberTestUtil.Builder()
            .withRole(RegulatorTeamRole.SCAP_VIEWER)
            .build());
    when(teamService.getTeamsThatUserBelongsTo(user)).thenReturn(List.of(regulatorTeam));
    mockMvc.perform(
            get(ReverseRouter.route(on(TeamManagementController.class).renderTeamList()))
                .with(user(user)))
        .andExpect(status().isOk())
        .andExpect(model().attribute("allTeams", List.of(TeamView.fromTeam(regulatorTeam))));
  }

  @Test
  void renderNewTeamForm_authorisedUser_isOk() throws Exception {
    var user = ServiceUserDetailTestUtil.Builder().build();

    mockMvc.perform(
        get(ReverseRouter.route(on(TeamManagementController.class).renderNewIndustryTeamForm(new NewTeamForm())))
            .with(user(user)))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/permissionmanagement/addTeam"))
    ;
  }

  @Test
  void addNewIndustryTeam_FormValidates_ReturnToTeamList() throws Exception {
    var user = ServiceUserDetailTestUtil.Builder().withWuaId(100L).build();

    var orgGroup = new OrganisationGroup();
    orgGroup.setName("Royal Dutch Shell");
    orgGroup.setOrganisationGroupId(10000);

    var form = new NewTeamForm();
    form.setOrganisationGroupId("10000");

    when(teamService.validate(any(), any())).thenReturn(emptyBindingResult());
    when(organisationGroupService.getOrganisationGroupById(any(), any())).thenReturn(Optional.of(orgGroup));when(userDetailService.getUserDetail()).thenReturn(user);
    when(userDetailService.getUserDetail()).thenReturn(user);

    mockMvc.perform(
            post(ReverseRouter.route(on(TeamManagementController.class).addNewIndustryTeam(form, emptyBindingResult())))
                .flashAttr("form", form)
                .with(csrf())
                .with(user(user)))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/permissionmanagement/teamList"));
  }

  @Test
  void addNewIndustryTeam_FormValidatesGroupDoesntExist_ReturnToNewTeam() throws Exception {
    var user = ServiceUserDetailTestUtil.Builder().withWuaId(100L).build();

    var form = new NewTeamForm();
    form.setOrganisationGroupId("10000");

    when(teamService.validate(any(), any())).thenReturn(emptyBindingResult());
    when(userDetailService.getUserDetail()).thenReturn(user);

    mockMvc.perform(
            post(ReverseRouter.route(on(TeamManagementController.class).addNewIndustryTeam(form, emptyBindingResult())))
                .flashAttr("form", form)
                .with(csrf())
                .with(user(user)))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/permissionmanagement/addTeam"));
  }

  @Test
  void renderArchiveTeamConfirmation_userNotPermitted() throws Exception {
    var teamId = new TeamId(new UUID(1, 1));
    var user = ServiceUserDetailTestUtil.Builder().withWuaId(100L).build();

    when(teamMemberService.getAllPermissionsForUser(user)).thenReturn(List.of(RolePermission.GRANT_ROLES));
    doReturn(user).when(userDetailService).getUserDetail();

    mockMvc.perform(
        get(ReverseRouter.route(on(TeamManagementController.class).renderArchiveTeamConfirmation(teamId)))
            .with(csrf())
            .with(user(user)))
        .andExpect(status().is4xxClientError());
  }

  @Test
  void archiveTeamConfirmation_userPermitted() throws Exception {
    var teamId = new TeamId(new UUID(1, 1));
    var user = ServiceUserDetailTestUtil.Builder().withWuaId(100L).build();

    mockMvc.perform(
        get(ReverseRouter.route(on(TeamManagementController.class).renderArchiveTeamConfirmation(teamId)))
            .with(csrf())
            .with(user(user)))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/permissionmanagement/removeTeam"));
  }

  @Test
  void renderArchiveTeamConfirmation_userNotPermittedReroute() throws Exception {
    var teamId = new TeamId(new UUID(1, 1));
    var user = ServiceUserDetailTestUtil.Builder().withWuaId(100L).build();

    when(teamMemberService.getAllPermissionsForUser(user)).thenReturn(List.of(RolePermission.GRANT_ROLES));
    doReturn(user).when(userDetailService).getUserDetail();

    mockMvc.perform(
            get(ReverseRouter.route(on(TeamManagementController.class).renderArchiveTeamConfirmation(teamId)))
                .with(csrf())
                .with(user(user)))
        .andExpect(status().is4xxClientError());
  }

  @Test
  void archiveTeam_VerifyCalls() throws Exception {
    var user = ServiceUserDetailTestUtil.Builder().withWuaId(100L).build();
    var teamId = new TeamId(new UUID(1, 1));
    var team = TeamTestUtil
        .Builder()
        .withId(teamId.uuid())
        .build();
    when(teamService.getTeam(teamId)).thenReturn(team);

    mockMvc.perform(
            post(ReverseRouter.route(on(TeamManagementController.class).archiveTeam(teamId, null)))
                .with(csrf())
                .with(user(user)))
        .andExpect(status().is3xxRedirection())
        .andExpect(redirectUrl("/permission-management/"));

    verify(teamService).archiveTeam(team);
  }
}
