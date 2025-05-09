package uk.co.nstauthority.scap.permissionmanagement.endpointsecurity;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;
import static uk.co.nstauthority.scap.authentication.TestUserProvider.user;

import java.util.List;
import java.util.Optional;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.stereotype.Controller;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.servlet.ModelAndView;
import uk.co.nstauthority.scap.AbstractControllerTest;
import uk.co.nstauthority.scap.authentication.ServiceUserDetail;
import uk.co.nstauthority.scap.authentication.ServiceUserDetailTestUtil;
import uk.co.nstauthority.scap.endpointvalidation.annotations.HasAnyPermissionForTeam;
import uk.co.nstauthority.scap.endpointvalidation.annotations.IsMemberOfTeam;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.permissionmanagement.Team;
import uk.co.nstauthority.scap.permissionmanagement.TeamId;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberTestUtil;
import uk.co.nstauthority.scap.permissionmanagement.TeamTestUtil;
import uk.co.nstauthority.scap.permissionmanagement.TeamType;

@ContextConfiguration(classes = TeamManagementHandlerInterceptorTest.TestController.class)
class TeamManagementHandlerInterceptorTest extends AbstractControllerTest {

  private static final ServiceUserDetail user = ServiceUserDetailTestUtil.Builder().build();

  private final Team team = TeamTestUtil.Builder().build();

  private final TeamId teamId = new TeamId(team.getUuid());

  @BeforeEach
  void setup() {
    when(teamService.getTeam(teamId)).thenReturn(team);
  }

  @Test
  void preHandle_whenMethodHasNoSupportedAnnotations_thenOkResponse() throws Exception {
    mockMvc.perform(get(ReverseRouter.route(on(TeamManagementHandlerInterceptorTest.TestController.class)
            .noSupportedAnnotations()))
            .with(user(user)))
        .andExpect(status().isOk());
  }

  @Test
  void preHandle_whenMethodHasOtherAnnotations_thenOkResponse() throws Exception {
    when(teamService.getTeamsOfTypeThatUserBelongsTo(user, TeamType.REGULATOR))
        .thenReturn(List.of(team));
    when(userDetailService.getUserDetail()).thenReturn(user);
    when(teamMemberService.findTeamMember(any(), any()))
        .thenReturn(Optional.of(TeamMemberTestUtil.Builder().build()));

    mockMvc.perform(get(ReverseRouter.route(on(TeamManagementHandlerInterceptorTest.TestController.class)
            .otherAnnotations(teamId)))
            .with(user(user)))
        .andExpect(status().isOk());
  }

  @Test
  void preHandle_whenMethodHasAnnotationsButNotPartOfTeam_ForbiddenResponse() throws Exception {
    when(userDetailService.getUserDetail()).thenReturn(user);

    mockMvc.perform(get(ReverseRouter.route(on(TeamManagementHandlerInterceptorTest.TestController.class)
            .isMemberOfTeam(teamId)))
            .with(user(user)))
        .andExpect(status().isForbidden());
  }

  @Test
  void preHandle_whenMethodHasAnnotationButParameterEmpty_BadRequest() throws Exception {
    when(userDetailService.getUserDetail()).thenReturn(user);

    mockMvc.perform(get(ReverseRouter.route(on(TeamManagementHandlerInterceptorTest.TestController.class)
            .emptyAnnotation()))
            .with(user(user)))
        .andExpect(status().isBadRequest());
  }

  @Test
  void preHandle_whenMethodHasAnnotationB_isOk() throws Exception {
    when(userDetailService.getUserDetail()).thenReturn(user);
    when(teamMemberService.isMemberOfTeam(teamId, user)).thenReturn(true);

    mockMvc.perform(get(ReverseRouter.route(on(TeamManagementHandlerInterceptorTest.TestController.class)
            .isMemberOfTeam(teamId)))
            .with(user(user)))
        .andExpect(status().isOk());
  }

  @Test
  void preHandle_whenMethodIsForbidden_isForbidden() throws Exception {
    when(userDetailService.getUserDetail()).thenReturn(user);
    when(teamMemberService.isMemberOfTeam(teamId, user)).thenReturn(false);

    mockMvc.perform(get(ReverseRouter.route(on(TeamManagementHandlerInterceptorTest.TestController.class)
            .isMemberOfTeam(teamId)))
            .with(user(user)))
        .andExpect(status().isForbidden());
  }


  @Controller
  static class TestController {

    private static final String VIEW_NAME = "test-view";

    @GetMapping("/permission-management/no-supported-annotation")
    ModelAndView noSupportedAnnotations() {
      return new ModelAndView(VIEW_NAME);
    }

    @GetMapping("/permission-management/other-annotation/{teamId}")
    @HasAnyPermissionForTeam(permissions = {RolePermission.GRANT_ROLES})
    ModelAndView otherAnnotations(@PathVariable TeamId teamId) {
      return new ModelAndView(VIEW_NAME);
    }

    @GetMapping("/permission-management/empty/member-of-team")
    @IsMemberOfTeam(allowRegulatorAccess = true)
    ModelAndView emptyAnnotation() {
      return new ModelAndView(VIEW_NAME);
    }

    @GetMapping("/permission-management/with-is-member-of-team/${teamId}")
    @IsMemberOfTeam
    ModelAndView isMemberOfTeam(@PathVariable TeamId teamId) {
      return new ModelAndView(VIEW_NAME);
    }
  }
}
