package uk.co.nstauthority.scap.endpointvalidation.rules;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.Optional;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import uk.co.nstauthority.scap.endpointvalidation.annotations.ScapHasStatus;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.summary.ScapSummaryController;

@ExtendWith(MockitoExtension.class)
class ScapHasStatusRuleTest extends AbstractInterceptorRuleTest {

  @Mock
  ScapDetailService scapDetailService;

  @InjectMocks
  ScapHasStatusRule rule;

  @Test
  @DisplayName("Test rule passes when status is DRAFT")
  void validate_scapIsDraft_IsOK() throws NoSuchMethodException {
    doReturn(scapDetail).when(scapDetailService).getLatestScapDetailByScapOrThrow(scap);
    when(scapDetail.getStatus()).thenReturn(ScapDetailStatus.DRAFT);

    var annotation = getAnnotation(
        TestController.class.getDeclaredMethod("get"),
        ScapHasStatus.class
    );
    var interceptorResult = rule.check(
        annotation,
        request,
        response,
        userDetail,
        scap
    );

    assertTrue(interceptorResult.hasRulePassed());
    verifyNoInteractions(response);
  }

  @Test
  @DisplayName("Assert redirection when status is SUBMITTED")
  void validate_WhenScapSubmitted() throws NoSuchMethodException {
    when(scap.getScapId()).thenReturn(SCAP_ID);
    doReturn(scapDetail).when(scapDetailService).getLatestScapDetailByScapOrThrow(scap);
    when(scapDetail.getStatus()).thenReturn(ScapDetailStatus.SUBMITTED);

    var annotation = getAnnotation(
        TestController.class.getDeclaredMethod("get"),
        ScapHasStatus.class
    );
    var interceptorResult = rule.check(
        annotation,
        request,
        response,
        userDetail,
        scap
    );
    var redirectUrl = ReverseRouter.route(on(ScapSummaryController.class).getScapSummary(SCAP_ID));

    assertFalse(interceptorResult.hasRulePassed());
    assertThat(interceptorResult.redirectUrl()).isEqualTo(redirectUrl);
  }

  @Test
  @DisplayName("Assert 400 when status is null")
  void validate_WhenScapStatusNull() throws NoSuchMethodException {
    doReturn(scapDetail).when(scapDetailService).getLatestScapDetailByScapOrThrow(scap);
    when(scapDetail.getStatus()).thenReturn(null);

    var annotation = getAnnotation(
        TestController.class.getDeclaredMethod("get"),
        ScapHasStatus.class
    );
    var interceptorResult = rule.check(
        annotation,
        request,
        response,
        userDetail,
        scap
    );

    assertFalse(interceptorResult.hasRulePassed());
    assertThat(interceptorResult.failureStatus()).isEqualTo(HttpStatus.BAD_REQUEST);
  }

  @Test
  @DisplayName("Assert that annotation works at class level")
  void validate_WhenScapIsDraft() {
    doReturn(scapDetail).when(scapDetailService).getLatestScapDetailByScapOrThrow(scap);
    when(scapDetail.getStatus()).thenReturn(ScapDetailStatus.SUBMITTED);

    var annotation = getAnnotation(
        TestController.class,
        ScapHasStatus.class
    );
    var interceptorResult = rule.check(
        annotation,
        request,
        response,
        userDetail,
        scap
    );

    assertTrue(interceptorResult.hasRulePassed());
    verifyNoInteractions(response);
  }

  @Controller
  @RequestMapping("/route1")
  @ScapHasStatus(permittedStatuses = ScapDetailStatus.SUBMITTED)
  private static class TestController {
    private static final String ENDPOINT_DATA = "Hello world!";

    @GetMapping
    @ScapHasStatus(permittedStatuses = ScapDetailStatus.DRAFT)
    String get() {
      return ENDPOINT_DATA;
    }

    @GetMapping("/route2")
    String getWithClassAnnotation() {
      return ENDPOINT_DATA;
    }
  }
}
