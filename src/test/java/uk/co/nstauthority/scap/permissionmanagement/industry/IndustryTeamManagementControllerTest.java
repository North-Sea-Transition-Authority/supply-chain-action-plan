package uk.co.nstauthority.scap.permissionmanagement.industry;

import static org.mockito.Mockito.when;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.csrf;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.model;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.view;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;
import static uk.co.nstauthority.scap.permissionmanagement.regulator.RegulatorTeamRole.ORGANISATION_ACCESS_MANAGER;

import java.util.List;
import java.util.Set;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.ApplicationContext;
import org.springframework.test.context.ContextConfiguration;
import uk.co.nstauthority.scap.error.exception.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.TeamId;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberViewTestUtil;
import uk.co.nstauthority.scap.permissionmanagement.TeamTestUtil;
import uk.co.nstauthority.scap.permissionmanagement.TeamType;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamManagementController;

@ContextConfiguration(classes = IndustryTeamMemberController.class)
class IndustryTeamManagementControllerTest extends AbstractIndustryTeamControllerTest {
  @MockBean
  private IndustryTeamService industryTeamService;

  @Autowired
  private ApplicationContext applicationContext;


  @Test
  void renderMemberList_whenNotAuthenticated_thenUnauthorised() throws Exception {
    mockMvc.perform(
      get(ReverseRouter.route(on(IndustryTeamMemberController.class).renderMemberList(teamId))))
      .andExpect(status().is3xxRedirection());
  }

  @Test
  void renderMemberList_whenMemberOfTeam_thenOk() throws Exception {
    mockMvc.perform(
      get(ReverseRouter.route(on(IndustryTeamMemberController.class).renderMemberList(teamId)))
        .with(authenticatedScapUser())
        .with(csrf()))
        .andExpect(status().isOk());
  }

  @Test
  void renderMemberList_whenNoTeamFound_thenNotFound() throws Exception {
    when(teamService.getTeam(teamId)).thenThrow(ScapEntityNotFoundException.class);

    mockMvc.perform(
      get(ReverseRouter.route(on(IndustryTeamMemberController.class).renderMemberList(teamId)))
        .with(authenticatedScapUser()))
        .andExpect(status().isNotFound());
  }

  @Test
  void renderMemberList_whenNotAccessManager_assertModelProperties() throws Exception {
    var teamMemberView = TeamMemberViewTestUtil.Builder()
      .withRole(IndustryTeamRole.ACCESS_MANAGER)
      .build();
    when(teamMemberViewService.findTeamMemberViewsForTeam(team)).thenReturn(List.of(teamMemberView));

    mockMvc.perform(
      get(ReverseRouter.route(on(IndustryTeamMemberController.class).renderMemberList(teamId)))
        .with(authenticatedScapUser()))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/permissionmanagement/teamMembers"))
        .andExpect(model().attribute("pageTitle", "Manage %s".formatted(team.getDisplayName())))
        .andExpect(model().attribute("teamName", team.getDisplayName()))
        .andExpect(model().attribute("teamRoles", IndustryTeamRole.values()))
        .andExpect(model().attributeDoesNotExist("addTeamMemberUrl"));
  }

  @Test
  void renderMemberList_whenAccessManager_assertModelProperties() throws Exception {
    var canRemoveUsers = true;
    var teamMemberView = TeamMemberViewTestUtil.Builder()
      .withRoles(Set.of(IndustryTeamRole.ACCESS_MANAGER))
      .build();

    var expectedAddUrl = ReverseRouter.route(on(IndustryAddMemberController.class).renderAddTeamMember(teamId));

    when(industryTeamService.isAccessManager(teamId, testUser)).thenReturn(true);
    when(teamMemberViewService.findTeamMemberViewsForTeam(team)).thenReturn(List.of(teamMemberView));
    when(teamMemberService.isMemberOfTeamWithAnyRoleOf(teamId, testUser, Set.of(IndustryTeamRole.ACCESS_MANAGER.name())))
      .thenReturn(canRemoveUsers);

    mockMvc.perform(
      get(ReverseRouter.route(on(IndustryTeamMemberController.class).renderMemberList(teamId)))
        .with(authenticatedScapUser()))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/permissionmanagement/teamMembers"))
        .andExpect(model().attribute("pageTitle", "Manage %s".formatted(team.getDisplayName())))
        .andExpect(model().attribute("teamName", team.getDisplayName()))
        .andExpect(model().attribute("teamRoles", IndustryTeamRole.values()))
        .andExpect(model().attribute("teamMembers", List.of(teamMemberView)))
        .andExpect(model().attribute("canRemoveUsers", true))
        .andExpect(model().attribute("canEditUsers", true))
        .andExpect(model().attribute("addTeamMemberUrl", expectedAddUrl));
  }

  @Test
  void renderMemberList_whenOrgAccessManager_assertModelProperties() throws Exception {
    var canRemoveUsers = true;
    var teamMemberView = TeamMemberViewTestUtil.Builder()
        .withRoles(Set.of(IndustryTeamRole.ACCESS_MANAGER))
        .build();

    var regulatorTeam = TeamTestUtil.Builder()
        .withTeamType(TeamType.REGULATOR)
        .build();

    when(teamService.getTeamsOfTypeThatUserBelongsTo(testUser, TeamType.REGULATOR)).thenReturn(List.of(regulatorTeam));
    when(teamMemberService.isMemberOfTeamWithAnyRoleOf(new TeamId(regulatorTeam.getUuid()),
        testUser,
        Set.of(ORGANISATION_ACCESS_MANAGER.name())))
        .thenReturn(true);
    when(teamMemberViewService.findTeamMemberViewsForTeam(team)).thenReturn(List.of(teamMemberView));
    when(teamMemberService.isMemberOfTeamWithAnyRoleOf(teamId, testUser, Set.of(IndustryTeamRole.ACCESS_MANAGER.name())))
        .thenReturn(canRemoveUsers);

    mockMvc.perform(
            get(ReverseRouter.route(on(IndustryTeamMemberController.class).renderMemberList(teamId)))
                .with(authenticatedScapUser()))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/permissionmanagement/teamMembers"))
        .andExpect(model().attribute("pageTitle", "Manage %s".formatted(team.getDisplayName())))
        .andExpect(model().attribute("teamName", team.getDisplayName()))
        .andExpect(model().attribute("teamRoles", IndustryTeamRole.values()))
        .andExpect(model().attribute("teamMembers", List.of(teamMemberView)))
        .andExpect(model().attribute("removeTeamUrl",
            ReverseRouter.route(on(TeamManagementController.class).renderArchiveTeamConfirmation(teamId))));
  }
}
