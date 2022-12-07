package uk.co.nstauthority.scap.permissionmanagement.industry;

import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.model;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.view;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;
import static uk.co.nstauthority.scap.authentication.TestUserProvider.user;

import java.util.List;
import java.util.Set;
import java.util.UUID;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.ApplicationContext;
import org.springframework.test.context.ContextConfiguration;
import uk.co.nstauthority.scap.AbstractControllerTest;
import uk.co.nstauthority.scap.authentication.ServiceUserDetailTestUtil;
import uk.co.nstauthority.scap.branding.CustomerConfigurationProperties;
import uk.co.nstauthority.scap.error.exception.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.TeamId;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberViewService;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberViewTestUtil;
import uk.co.nstauthority.scap.permissionmanagement.TeamTestUtil;
import uk.co.nstauthority.scap.permissionmanagement.TeamType;

@ContextConfiguration(classes = IndustryTeamManagementController.class)
class IndustryTeamManagementControllerTest extends AbstractControllerTest {

  @MockBean
  private TeamMemberViewService teamMemberViewService;

  @MockBean
  private IndustryTeamService industryTeamService;

  @Autowired
  private ApplicationContext applicationContext;

  @Test
  void renderMemberList_whenNotAuthenticated_thenUnauthorised() throws Exception {
    var teamId = new TeamId(UUID.randomUUID());

    mockMvc.perform(
      get(ReverseRouter.route(on(IndustryTeamManagementController.class).renderMemberList(teamId))))
      .andExpect(status().isUnauthorized());
  }

  @Test
  void renderMemberList_whenMemberOfTeam_thenOk() throws Exception {
    var user = ServiceUserDetailTestUtil.Builder().build();
    var team = TeamTestUtil.Builder()
      .withTeamType(TeamType.INDUSTRY)
      .build();
    var teamId = new TeamId(team.getUuid());

    when(teamMemberService.isMemberOfTeam(teamId, user)).thenReturn(true);
    when(industryTeamService.getTeamOrThrow(teamId)).thenReturn(team);

    mockMvc.perform(
      get(ReverseRouter.route(on(IndustryTeamManagementController.class).renderMemberList(teamId)))
        .with(user(user)))
        .andExpect(status().isOk());
  }

  @Test
  void renderMemberList_whenNoTeamFound_thenNotFound() throws Exception {
    var user = ServiceUserDetailTestUtil.Builder().build();
    var teamId = new TeamId(UUID.randomUUID());

    when(teamMemberService.isMemberOfTeam(teamId, user)).thenReturn(true);
    when(industryTeamService.getTeamOrThrow(teamId)).thenThrow(ScapEntityNotFoundException.class);

    mockMvc.perform(
      get(ReverseRouter.route(on(IndustryTeamManagementController.class).renderMemberList(teamId)))
        .with(user(user)))
        .andExpect(status().isNotFound());
  }

  @Test
  void renderMemberList_whenNotAccessManager_assertModelProperties() throws Exception {
    var user = ServiceUserDetailTestUtil.Builder().build();
    var team = TeamTestUtil.Builder()
      .withTeamType(TeamType.INDUSTRY)
      .build();
    var teamId = new TeamId(team.getUuid());

    when(teamMemberService.isMemberOfTeam(teamId, user)).thenReturn(true);
    when(industryTeamService.getTeamOrThrow(teamId)).thenReturn(team);

    var teamMemberView = TeamMemberViewTestUtil.Builder()
      .withRole(IndustryTeamRole.ACCESS_MANAGER)
      .build();

    when(teamMemberViewService.getTeamMemberViewsForTeam(team)).thenReturn(List.of(teamMemberView));

    var mnemonic = applicationContext.getBean(CustomerConfigurationProperties.class).mnemonic();

    mockMvc.perform(
      get(ReverseRouter.route(on(IndustryTeamManagementController.class).renderMemberList(teamId)))
        .with(user(user)))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/permissionmanagement/teamMembers"))
        .andExpect(model().attribute("pageTitle", "Manage %s".formatted(team.getDisplayName())))
        .andExpect(model().attribute("teamName", team.getDisplayName()))
        .andExpect(model().attribute("teamRoles", IndustryTeamRole.values()))
        .andExpect(model().attributeDoesNotExist("addTeamMemberUrl"));
  }

  @Test
  void renderMemberList_whenAccessManager_assertModelProperties() throws Exception {
    var user = ServiceUserDetailTestUtil.Builder().build();
    var team = TeamTestUtil.Builder()
      .withTeamType(TeamType.INDUSTRY)
      .build();
    var teamId = new TeamId(team.getUuid());

    when(industryTeamService.isAccessManager(teamId, user)).thenReturn(true);
    when(teamMemberService.isMemberOfTeam(teamId, user)).thenReturn(true);
    when(industryTeamService.getTeamOrThrow(teamId)).thenReturn(team);

    var canRemoveUsers = true;
    var teamMemberView = TeamMemberViewTestUtil.Builder()
      .withRoles(Set.of(IndustryTeamRole.ACCESS_MANAGER))
      .build();

    when(teamMemberViewService.getTeamMemberViewsForTeam(team)).thenReturn(List.of(teamMemberView));
    when(teamMemberService.isMemberOfTeamWithAnyRoleOf(teamId, user, Set.of(IndustryTeamRole.ACCESS_MANAGER.name())))
      .thenReturn(canRemoveUsers);

    var mnemonic = applicationContext.getBean(CustomerConfigurationProperties.class).mnemonic();

    mockMvc.perform(
      get(ReverseRouter.route(on(IndustryTeamManagementController.class).renderMemberList(teamId)))
        .with(user(user)))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/permissionmanagement/teamMembers"))
        .andExpect(model().attribute("pageTitle", "Manage %s".formatted(team.getDisplayName())))
        .andExpect(model().attribute("teamName", team.getDisplayName()))
        .andExpect(model().attribute("teamRoles", IndustryTeamRole.values()))
        .andExpect(model().attribute("teamMembers", List.of(teamMemberView)));
  }
}