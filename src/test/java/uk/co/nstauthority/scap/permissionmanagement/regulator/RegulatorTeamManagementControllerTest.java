package uk.co.nstauthority.scap.permissionmanagement.regulator;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.model;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.view;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;
import static uk.co.nstauthority.scap.authentication.TestUserProvider.user;

import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.ApplicationContext;
import org.springframework.test.context.ContextConfiguration;
import uk.co.nstauthority.scap.AbstractControllerTest;
import uk.co.nstauthority.scap.authentication.ServiceUserDetail;
import uk.co.nstauthority.scap.authentication.ServiceUserDetailTestUtil;
import uk.co.nstauthority.scap.branding.CustomerConfigurationProperties;
import uk.co.nstauthority.scap.error.exception.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.TeamId;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberTestUtil;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberViewService;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberViewTestUtil;
import uk.co.nstauthority.scap.permissionmanagement.TeamTestUtil;
import uk.co.nstauthority.scap.permissionmanagement.TeamType;
import uk.co.nstauthority.scap.permissionmanagement.industry.IndustryTeamRole;

@ContextConfiguration(classes = RegulatorTeamManagementController.class)
class RegulatorTeamManagementControllerTest extends AbstractControllerTest {

  @MockBean
  private TeamMemberViewService teamMemberViewService;

  @MockBean
  private RegulatorTeamService regulatorTeamService;

  @Autowired
  private ApplicationContext applicationContext;

  private final ServiceUserDetail user = getUser();

  @BeforeEach
  void setup() {
    when(userDetailService.getUserDetail()).thenReturn(user);
    when(teamMemberService.findTeamMember(any(), any()))
        .thenReturn(Optional.of(TeamMemberTestUtil.Builder()
            .withRole(IndustryTeamRole.ACCESS_MANAGER)
            .build()));
    when(teamMemberService.isMemberOfTeam(any(), any())).thenReturn(true);
  }

  @Test
  void renderMemberListRedirect_whenNotAuthenticated_thenUnauthorised() throws Exception {
    mockMvc.perform(
      get(ReverseRouter.route(on(RegulatorTeamManagementController.class).renderMemberListRedirect())))
      .andExpect(status().is3xxRedirection());
  }

  @Test
  void renderMemberList_whenNotAuthenticated_thenUnauthorised() throws Exception {
    var teamId = new TeamId(UUID.randomUUID());

    mockMvc.perform(
      get(ReverseRouter.route(on(RegulatorTeamManagementController.class).renderMemberList(teamId))))
      .andExpect(status().is3xxRedirection());
  }

  @Test
  void renderMemberList_whenMemberOfTeam_thenOk() throws Exception {
    var user = ServiceUserDetailTestUtil.Builder().build();
    var team = TeamTestUtil.Builder()
      .withTeamType(TeamType.REGULATOR)
      .build();
    var teamId = new TeamId(team.getUuid());

    when(teamMemberService.isMemberOfTeam(teamId, user)).thenReturn(true);
    when(teamService.getTeam(teamId)).thenReturn(team);

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
    when(teamService.getTeam(teamId)).thenThrow(ScapEntityNotFoundException.class);

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
    when(teamService.getTeam(teamId)).thenReturn(team);

    var teamMemberView = TeamMemberViewTestUtil.Builder()
      .withRole(RegulatorTeamRole.ACCESS_MANAGER)
      .build();

    when(teamMemberViewService.getTeamMemberViewsForTeam(team)).thenReturn(List.of(teamMemberView));

    var mnemonic = applicationContext.getBean(CustomerConfigurationProperties.class).mnemonic();

    mockMvc.perform(
      get(ReverseRouter.route(on(RegulatorTeamManagementController.class).renderMemberList(teamId)))
        .with(user(user)))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/permissionmanagement/teamMembers"))
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
    when(teamService.getTeam(teamId)).thenReturn(team);

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
        .andExpect(view().name("scap/permissionmanagement/teamMembers"))
        .andExpect(model().attribute("pageTitle", "Manage %s".formatted(mnemonic)))
        .andExpect(model().attribute("teamName", mnemonic))
        .andExpect(model().attribute("teamRoles", RegulatorTeamRole.values()))
        .andExpect(model().attribute("teamMembers", List.of(teamMemberView)));
  }

  private ServiceUserDetail getUser() {
    return ServiceUserDetailTestUtil.Builder()
        .withWuaId(100L)
        .build();
  }
}