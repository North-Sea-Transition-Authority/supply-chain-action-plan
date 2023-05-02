package uk.co.nstauthority.scap.permissionmanagement.endpointsecurity;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.springframework.stereotype.Controller;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.servlet.ModelAndView;
import uk.co.nstauthority.scap.AbstractControllerTest;
import uk.co.nstauthority.scap.endpointvalidation.annotations.HasAnyPermissionForScap;
import uk.co.nstauthority.scap.error.exception.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.permissionmanagement.Team;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberTestUtil;
import uk.co.nstauthority.scap.permissionmanagement.TeamTestUtil;
import uk.co.nstauthority.scap.permissionmanagement.TeamType;
import uk.co.nstauthority.scap.permissionmanagement.industry.IndustryTeamRole;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.scap.Scap;
import uk.co.nstauthority.scap.scap.scap.ScapId;

@ContextConfiguration(classes = ScapPermissionManagementHandlerInterceptorTest.TestController.class)
class ScapPermissionManagementHandlerInterceptorTest extends AbstractControllerTest {

  private static final ScapId scapId = new ScapId(10000);

  @Test
  void preHandle_whenMethodHasNoSupportedAnnotations_thenOkResponse() throws Exception {
    mockMvc.perform(get(ReverseRouter.route(on(TestController.class)
            .noSupportedAnnotations()
        ))
            .with(authenticatedScapUser()))
        .andExpect(status().isOk());
  }

  @Test
  void preHandle_whenMethodHasScapPermissionButNoScap_thenBadRequest() throws Exception {
    mockMvc.perform(get(ReverseRouter.route(on(TestController.class)
        .withManageOrganisation()))
        .with(authenticatedScapUser()))
        .andExpect(status().isBadRequest());
  }

  @Test
  void preHandle_whenMethodHasScapPermissionButScapHasNoTeam_thenNotFound() throws Exception {
    when(scapService.getScapById(scapId)).thenReturn(getTestScap());
    when(scapDetailService.getActionableScapDetail(scapId, testUser)).thenReturn(getTestDetail());
    when(teamService.getByEnergyPortalOrgGroupId(anyInt())).thenThrow(new ScapEntityNotFoundException("TEST"));
    mockMvc.perform(get(ReverseRouter.route(on(TestController.class)
            .withManageOrganisationAndTeam(scapId)))
            .with(authenticatedScapUser()))
        .andExpect(status().isNotFound());
  }

  @Test
  void preHandle_whenMethodHasScapPermissionButUserIsNotTeamMember_thenNotFound() throws Exception {
    when(scapDetailService.getActionableScapDetail(scapId, testUser)).thenReturn(getTestDetail());
    when(teamService.getByEnergyPortalOrgGroupId(anyInt())).thenReturn(TeamTestUtil.Builder().build());
    mockMvc.perform(get(ReverseRouter.route(on(TestController.class)
            .withManageOrganisationAndTeam(scapId)))
            .with(authenticatedScapUser()))
        .andExpect(status().isForbidden());
  }

  @Test
  void preHandle_whenMethodHasScapPermissionButUserHasNotPermission_thenForbidden() throws Exception {
    when(scapService.getScapById(scapId)).thenReturn(getTestScap());
    when(scapDetailService.getActionableScapDetail(scapId, testUser)).thenReturn(getTestDetail());
    when(teamService.getByEnergyPortalOrgGroupId(anyInt())).thenReturn(TeamTestUtil.Builder().build());
    when(teamMemberService.getTeamMember(any(Team.class), any())).thenReturn(TeamMemberTestUtil.Builder().build());
    mockMvc.perform(get(ReverseRouter.route(on(TestController.class)
            .withManageOrganisationAndTeam(scapId)))
            .with(authenticatedScapUser()))
        .andExpect(status().isForbidden());
  }

  @Test
  void preHandle_authorised_thenOk() throws Exception {
    var teamMember = TeamMemberTestUtil
        .Builder()
        .withTeamType(TeamType.INDUSTRY)
        .withRole(IndustryTeamRole.SCAP_SUBMITTER)
        .build();

    var team = TeamTestUtil.Builder().build();
    when(scapService.getScapById(scapId)).thenReturn(getTestScap());
    when(scapDetailService.getActionableScapDetail(scapId, testUser)).thenReturn(getTestDetail());
    when(teamService.getByEnergyPortalOrgGroupId(anyInt())).thenReturn(team);
    when(teamMemberService.findTeamMember(team, testUser.getWebUserAccountId())).thenReturn(Optional.ofNullable(teamMember));
    mockMvc.perform(get(ReverseRouter.route(on(TestController.class)
            .withManageOrganisationAndTeam(scapId)))
            .with(authenticatedScapUser()))
        .andExpect(status().isOk());
  }

  private Scap getTestScap() {
    var scap = new Scap();
    scap.setOrganisationGroupId(1000);
    return scap;
  }

  private ScapDetail getTestDetail() {
    var scapDetail = new ScapDetail();
    scapDetail.setScap(getTestScap());

    return scapDetail;
  }


  @Controller
  static class TestController {

    private static final String VIEW_NAME = "test-view";

    @GetMapping("/permission-management/no-supported-annotation")
    ModelAndView noSupportedAnnotations() {
      return new ModelAndView(VIEW_NAME);
    }

    @GetMapping("/permission-management/with-permission-manage-organisation")
    @HasAnyPermissionForScap(permissions = RolePermission.SUBMIT_SCAP)
    ModelAndView withManageOrganisation() {
      return new ModelAndView(VIEW_NAME);
    }

    @GetMapping("/permission-management/with-permission-manage-organisation/${scapId}")
    @HasAnyPermissionForScap(allowRegulatorAccess = true, permissions = RolePermission.SUBMIT_SCAP)
    ModelAndView withManageOrganisationAndTeam(@PathVariable ScapId scapId) {
      return new ModelAndView(VIEW_NAME);
    }
  }
}
