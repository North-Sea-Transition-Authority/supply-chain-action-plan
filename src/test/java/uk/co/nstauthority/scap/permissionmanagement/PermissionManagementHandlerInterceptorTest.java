package uk.co.nstauthority.scap.permissionmanagement;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;
import static uk.co.nstauthority.scap.authentication.TestUserProvider.user;

import java.util.List;
import java.util.Optional;
import java.util.UUID;
import org.junit.jupiter.api.Test;
import org.springframework.stereotype.Controller;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.servlet.ModelAndView;
import uk.co.nstauthority.scap.AbstractControllerTest;
import uk.co.nstauthority.scap.authentication.ServiceUserDetail;
import uk.co.nstauthority.scap.authentication.ServiceUserDetailTestUtil;
import uk.co.nstauthority.scap.energyportal.WebUserAccountId;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.regulator.RegulatorTeamRole;

@ContextConfiguration(classes = PermissionManagementHandlerInterceptorTest.TestController.class)
class PermissionManagementHandlerInterceptorTest extends AbstractControllerTest {
  private static final ServiceUserDetail USER = ServiceUserDetailTestUtil.Builder().build();

  private static final TeamId teamId = new TeamId(UUID.randomUUID());


  @Test
  void preHandle_whenMethodHasNoSupportedAnnotations_thenOkResponse() throws Exception {
    mockMvc.perform(get(ReverseRouter.route(on(TestController.class)
            .noSupportedAnnotations()
        ))
            .with(user(USER)))
        .andExpect(status().isOk());
  }

  @Test
  void preHandle_whenMethodHasOtherAnnotation_thenOkResponse() throws Exception {
    when(userDetailService.getUserDetail()).thenReturn(USER);
    when(teamMemberService.isMemberOfTeam(teamId, USER)).thenReturn(true);

    mockMvc.perform(get(ReverseRouter.route(on(TestController.class)
            .hasOtherAnnotations(teamId)
        ))
            .with(user(USER)))
        .andExpect(status().isOk());
  }

  @Test
  void whenMethodHasPermissionRequired_notTeamFound_BadRequest() throws Exception {
    mockMvc.perform(get(ReverseRouter.route(on(TestController.class)
            .withManageOrganisation()
        ))
            .with(user(USER)))
        .andExpect(status().isBadRequest());
  }

  @Test
  void whenMethodHasPermissionRequired_whenRegulatorWithoutPermission_thenForbidden() throws Exception {
    var team = TeamTestUtil.Builder().build();
    var teamMember = TeamMemberTestUtil.Builder().build();
    when(teamService.findTeam(teamId)).thenReturn(Optional.of(TeamTestUtil.Builder().build()));
    when(userDetailService.getUserDetail()).thenReturn(USER);
    when(teamService.getTeamsOfTypeThatUserBelongsTo(USER, TeamType.REGULATOR))
        .thenReturn(List.of(team));
    when(teamMemberService.getTeamMember(team, new WebUserAccountId(USER.wuaId())))
        .thenReturn(Optional.of(teamMember));

    mockMvc.perform(get(ReverseRouter.route(on(TestController.class)
            .withManageOrganisationAndTeam(teamId)
        ))
            .with(user(USER)))
        .andExpect(status().isForbidden());
  }

  @Test
  void whenMethodHasPermissionRequired_whenRegulatorWithoutTeam_thenForbidden() throws Exception {
    var team = TeamTestUtil.Builder().build();
    var teamMember = TeamMemberTestUtil
        .Builder()
        .withRole(RegulatorTeamRole.ORGANISATION_ACCESS_MANAGER).build();
    when(teamService.findTeam(teamId)).thenReturn(Optional.of(TeamTestUtil.Builder().build()));
    when(userDetailService.getUserDetail()).thenReturn(USER);
    when(teamService.getTeamsOfTypeThatUserBelongsTo(USER, TeamType.REGULATOR))
        .thenReturn(List.of(team));
    when(teamMemberService.getTeamMember(team, new WebUserAccountId(USER.wuaId())))
        .thenReturn(Optional.empty());

    mockMvc.perform(get(ReverseRouter.route(on(TestController.class)
            .withManageOrganisationAndTeam(teamId)
        ))
            .with(user(USER)))
        .andExpect(status().isForbidden());
  }

  @Test
  void whenMethodHasPermissionRequired_whenRegulatorWithPermission_thenOk() throws Exception {
    var team = TeamTestUtil.Builder().build();
    var teamMember = TeamMemberTestUtil
        .Builder()
        .withRole(RegulatorTeamRole.ORGANISATION_ACCESS_MANAGER).build();
    when(teamService.findTeam(teamId)).thenReturn(Optional.of(TeamTestUtil.Builder().build()));
    when(userDetailService.getUserDetail()).thenReturn(USER);
    when(teamService.getTeamsOfTypeThatUserBelongsTo(USER, TeamType.REGULATOR))
        .thenReturn(List.of(team));
    when(teamMemberService.getTeamMember(team, new WebUserAccountId(USER.wuaId())))
        .thenReturn(Optional.of(teamMember));

    mockMvc.perform(get(ReverseRouter.route(on(TestController.class)
            .withManageOrganisationAndTeam(teamId)
        ))
            .with(user(USER)))
        .andExpect(status().isOk());
  }

  @Test
  void whenMethodHasPermissionRequired_whenIndustryInTeamWithPermission_thenOk() throws Exception {
    var team = TeamTestUtil.Builder().build();
    var teamMember = TeamMemberTestUtil
        .Builder()
        .withRole(RegulatorTeamRole.ORGANISATION_ACCESS_MANAGER).build();
    when(teamService.findTeam(teamId)).thenReturn(Optional.of(team));
    when(userDetailService.getUserDetail()).thenReturn(USER);
    when(teamService.getTeam(teamId)).thenReturn(team).thenReturn(team);
    when(teamMemberService.getTeamMember(team, new WebUserAccountId(USER.wuaId())))
        .thenReturn(Optional.of(teamMember));

    mockMvc.perform(get(ReverseRouter.route(on(TestController.class)
            .withManageOrganisationAndTeam(teamId)
        ))
            .with(user(USER)))
        .andExpect(status().isOk());
  }

  @Test
  void whenMethodHasPermissionRequired_whenIndustryInTeamWithoutPermission_thenForbidden() throws Exception {
    var team = TeamTestUtil.Builder().build();
    var teamMember = TeamMemberTestUtil
        .Builder()
        .withRole(RegulatorTeamRole.SCAP_VIEWER).build();
    when(teamService.findTeam(teamId)).thenReturn(Optional.of(team));
    when(userDetailService.getUserDetail()).thenReturn(USER);
    when(teamService.getTeam(teamId)).thenReturn(team).thenReturn(team);
    when(teamMemberService.getTeamMember(team, new WebUserAccountId(USER.wuaId())))
        .thenReturn(Optional.of(teamMember));

    mockMvc.perform(get(ReverseRouter.route(on(TestController.class)
            .withManageOrganisationAndTeam(teamId)
        ))
            .with(user(USER)))
        .andExpect(status().isForbidden());
  }

  @Controller
  static class TestController {

    private static final String VIEW_NAME = "test-view";

    @GetMapping("/permission-management/no-supported-annotation")
    ModelAndView noSupportedAnnotations() {
      return new ModelAndView(VIEW_NAME);
    }

    @GetMapping("/permission-management/has-other-annotation/{teamId}")
    @IsMemberOfTeamOrRegulator
    ModelAndView hasOtherAnnotations(@PathVariable TeamId teamId) {
      return new ModelAndView(VIEW_NAME);
    }

    @GetMapping("/permission-management/with-permission-manage-organisation")
    @PermissionsRequired(permissions = RolePermission.MANAGE_ORGANISATIONS)
    ModelAndView withManageOrganisation() {
      return new ModelAndView(VIEW_NAME);
    }

    @GetMapping("/permission-management/with-permission-manage-organisation/${teamId}")
    @PermissionsRequired(permissions = RolePermission.MANAGE_ORGANISATIONS)
    ModelAndView withManageOrganisationAndTeam(@PathVariable TeamId teamId) {
      return new ModelAndView(VIEW_NAME);
    }
  }
}
