package uk.co.nstauthority.scap.endpointvalidation.rules;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.List;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import uk.co.nstauthority.scap.endpointvalidation.annotations.ScapHasStatus;
import uk.co.nstauthority.scap.endpointvalidation.annotations.UserHasAnyPermission;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamManagementController;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;

class UserHasPermissionRuleTest extends AbstractInterceptorRuleTest {

  @Mock
  TeamMemberService teamMemberService;
  @InjectMocks
  UserHasAnyPermissionRule rule;

  @Test
  void userHasPermission_supportsAnnotation() {
    assertThat(rule.supports()).isEqualTo(UserHasAnyPermission.class);
  }

  @Test
  void userHasPermission_continueAsNormal() throws NoSuchMethodException {
    when(teamMemberService.getAllPermissionsForUser(userDetail)).thenReturn(List.of(RolePermission.MANAGE_ORGANISATIONS));
    var annotation = getAnnotation(
        UserHasPermissionRuleTest.TestController.class.getDeclaredMethod("get"),
        UserHasAnyPermission.class
    );

    var interceptorResult = rule.check(
        annotation,
        request,
        response,
        userDetail,
        scapDetail,
        null
    );

    assertTrue(interceptorResult.hasRulePassed());
    verifyNoInteractions(response);
  }

  @Test
  void userDoesntHavePermission_continueAsRedirect() throws NoSuchMethodException {
    when(teamMemberService.getAllPermissionsForUser(userDetail)).thenReturn(List.of(RolePermission.GRANT_ROLES));
    var annotation = getAnnotation(
        UserHasPermissionRuleTest.TestController.class.getDeclaredMethod("get"),
        UserHasAnyPermission.class
    );

    var interceptorResult = rule.check(
        annotation,
        request,
        response,
        userDetail,
        scapDetail,
        null
    );

    var redirectUrl = ReverseRouter.route(on(TeamManagementController.class).renderTeamList());
    assertFalse(interceptorResult.hasRulePassed());
    assertThat(interceptorResult.failureStatus().getReasonPhrase()).isEqualTo("Forbidden");
  }

  @Controller
  @RequestMapping("/route1")
  @ScapHasStatus(permittedStatuses = ScapDetailStatus.SUBMITTED)
  private static class TestController {
    private static final String ENDPOINT_DATA = "Hello world!";

    @GetMapping
    @UserHasAnyPermission(permissions = RolePermission.MANAGE_ORGANISATIONS)
    String get() {
      return ENDPOINT_DATA;
    }
  }
}
