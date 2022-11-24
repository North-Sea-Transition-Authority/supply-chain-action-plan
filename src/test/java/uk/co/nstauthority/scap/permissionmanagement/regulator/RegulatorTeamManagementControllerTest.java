package uk.co.nstauthority.scap.permissionmanagement.regulator;

import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.model;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.redirectedUrl;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.view;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;
import static uk.co.nstauthority.scap.authentication.TestUserProvider.user;

import java.util.List;
import java.util.Optional;
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
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.TeamId;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberViewService;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberViewTestUtil;
import uk.co.nstauthority.scap.permissionmanagement.TeamTestUtil;
import uk.co.nstauthority.scap.permissionmanagement.TeamType;

@ContextConfiguration(classes = RegulatorTeamManagementController.class)
class RegulatorTeamManagementControllerTest extends AbstractControllerTest {

  @MockBean
  private TeamMemberViewService teamMemberViewService;

  @MockBean
  private RegulatorTeamService regulatorTeamService;

  @Autowired
  private ApplicationContext applicationContext;

  @Test
  void renderMemberListRedirect_whenNotAuthenticated_thenUnauthorised() throws Exception {
    mockMvc.perform(
      get(ReverseRouter.route(on(RegulatorTeamManagementController.class).renderMemberListRedirect())))
      .andExpect(status().isUnauthorized());
  }

  @Test
  void renderMemberListRedirect_whenNoAccessToRegulatorTeam_thenForbidden() throws Exception {
    var user = ServiceUserDetailTestUtil.Builder().build();

    when(regulatorTeamService.getRegulatorTeamForUser(user)).thenReturn(Optional.empty());

    mockMvc.perform(
      get(ReverseRouter.route(on(RegulatorTeamManagementController.class).renderMemberListRedirect()))
        .with(user(user)))
        .andExpect(status().isForbidden());
  }

  @Test
  void renderMemberList_whenNotAuthenticated_thenUnauthorised() throws Exception {
    var teamId = new TeamId(UUID.randomUUID());

    mockMvc.perform(
      get(ReverseRouter.route(on(RegulatorTeamManagementController.class).renderMemberList(teamId))))
      .andExpect(status().isUnauthorized());
  }

  @Test
  void renderMemberList_whenMemberOfTeam_thenOk() throws Exception {
    var user = ServiceUserDetailTestUtil.Builder().build();
    var team = TeamTestUtil.Builder()
      .withTeamType(TeamType.REGULATOR)
      .build();
    var teamId = new TeamId(team.getUuid());

    when(teamMemberService.isMemberOfTeam(teamId, user)).thenReturn(true);
    when(regulatorTeamService.getTeam(teamId)).thenReturn(Optional.of(team));

    mockMvc.perform(
      get(ReverseRouter.route(on(RegulatorTeamManagementController.class).renderMemberList(teamId)))
        .with(user(user)))
        .andExpect(status().isOk());
  }

  @Test
  void renderMemberList_whenNoTeamFound_thenNotFound() throws Exception {
    var user = ServiceUserDetailTestUtil.Builder().build();
    var teamId = new TeamId(UUID.randomUUID());

    when(teamMemberService.isMemberOfTeam(teamId, user)).thenReturn(true);
    when(regulatorTeamService.getTeam(teamId)).thenReturn(Optional.empty());

    mockMvc.perform(
      get(ReverseRouter.route(on(RegulatorTeamManagementController.class).renderMemberList(teamId)))
        .with(user(user)))
        .andExpect(status().isNotFound());
  }

  @Test
  void renderMemberList_whenNotAccessManager_assertModelProperties() throws Exception {
    var user = ServiceUserDetailTestUtil.Builder().build();
    var team = TeamTestUtil.Builder()
      .withTeamType(TeamType.REGULATOR)
      .build();
    var teamId = new TeamId(team.getUuid());

    when(teamMemberService.isMemberOfTeam(teamId, user)).thenReturn(true);
    when(regulatorTeamService.getTeam(teamId)).thenReturn(Optional.of(team));

    var teamMemberView = TeamMemberViewTestUtil.Builder()
      .withRole(RegulatorTeamRole.ACCESS_MANAGER)
      .build();

    when(teamMemberViewService.getTeamMemberViewsForTeam(team)).thenReturn(List.of(teamMemberView));

    var mnemonic = applicationContext.getBean(CustomerConfigurationProperties.class).mnemonic();

    mockMvc.perform(
      get(ReverseRouter.route(on(RegulatorTeamManagementController.class).renderMemberList(teamId)))
        .with(user(user)))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/permissionmanagement/regulator/regulatorTeamMembers"))
        .andExpect(model().attribute("pageTitle", "Manage %s".formatted(mnemonic)))
        .andExpect(model().attribute("teamName", mnemonic))
        .andExpect(model().attribute("teamRoles", RegulatorTeamRole.values()))
        .andExpect(model().attributeDoesNotExist("addTeamMemberUrl"));
  }

  @Test
  void renderMemberList_whenAccessManager_assertModelProperties() throws Exception {
    var user = ServiceUserDetailTestUtil.Builder().build();
    var team = TeamTestUtil.Builder()
      .withTeamType(TeamType.REGULATOR)
      .build();
    var teamId = new TeamId(team.getUuid());

    when(regulatorTeamService.isAccessManager(teamId, user)).thenReturn(true);
    when(teamMemberService.isMemberOfTeam(teamId, user)).thenReturn(true);
    when(regulatorTeamService.getTeam(teamId)).thenReturn(Optional.of(team));

    var canRemoveUsers = true;
    var teamMemberView = TeamMemberViewTestUtil.Builder()
      .withRoles(Set.of(RegulatorTeamRole.ACCESS_MANAGER))
      .build();

    when(teamMemberViewService.getTeamMemberViewsForTeam(team)).thenReturn(List.of(teamMemberView));
    when(teamMemberService.isMemberOfTeamWithAnyRoleOf(teamId, user, Set.of(RegulatorTeamRole.ACCESS_MANAGER.name())))
      .thenReturn(canRemoveUsers);

    var mnemonic = applicationContext.getBean(CustomerConfigurationProperties.class).mnemonic();

    mockMvc.perform(
      get(ReverseRouter.route(on(RegulatorTeamManagementController.class).renderMemberList(teamId)))
        .with(user(user)))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/permissionmanagement/regulator/regulatorTeamMembers"))
        .andExpect(model().attribute("pageTitle", "Manage %s".formatted(mnemonic)))
        .andExpect(model().attribute("teamName", mnemonic))
        .andExpect(model().attribute("teamRoles", RegulatorTeamRole.values()))
        .andExpect(model().attribute("teamMembers", List.of(teamMemberView)));
  }
}