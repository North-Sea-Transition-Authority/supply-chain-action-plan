package uk.co.nstauthority.scap.permissionmanagement;

import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;
import static uk.co.nstauthority.scap.authentication.TestUserProvider.user;

import java.util.List;
import org.junit.jupiter.api.Test;
import org.springframework.stereotype.Controller;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.servlet.ModelAndView;
import uk.co.nstauthority.scap.AbstractControllerTest;
import uk.co.nstauthority.scap.mvc.ReverseRouter;

@ContextConfiguration(classes = PermissionManagementHandlerInterceptorTest.TestController.class)
class PermissionManagementHandlerInterceptorTest extends AbstractControllerTest {

  @Test
  void preHandle_whenMethodHasNoSupportedAnnotations_thenOkResponse() throws Exception {
    mockMvc.perform(get(ReverseRouter.route(on(TestController.class)
            .noSupportedAnnotations()))
            .with(testUser()))
        .andExpect(status().isOk());
  }

  @Test
  void whenMethodHasPermissionRequired_whenWithoutPermission_thenForbidden() throws Exception {
    var permissions = List.of(RolePermission.VIEW_SCAP);
    when(teamMemberService.getAllPermissionsForUser(testUser)).thenReturn(permissions);
    when(userDetailService.getUserDetail()).thenReturn(testUser);

    mockMvc.perform(get(ReverseRouter.route(on(TestController.class)
            .withPermissionRequired()))
            .with(testUser()))
        .andExpect(status().isForbidden());
  }

  @Test
  void whenMethodHasPermissionRequired_whenWithPermission_thenOk() throws Exception {
    mockMvc.perform(get(ReverseRouter.route(on(TestController.class)
            .withPermissionRequired()))
            .with(testUser()))
        .andExpect(status().isOk());
  }

  @Controller
  static class TestController {

    private static final String VIEW_NAME = "test-view";

    @GetMapping("/permission-management/no-supported-annotation")
    ModelAndView noSupportedAnnotations() {
      return new ModelAndView(VIEW_NAME);
    }

    @GetMapping("/permission-management/with-permission-manage-organisation")
    @PermissionsRequired(permissions = RolePermission.SUBMIT_SCAP)
    ModelAndView withPermissionRequired() {
      return new ModelAndView(VIEW_NAME);
    }
  }
}
