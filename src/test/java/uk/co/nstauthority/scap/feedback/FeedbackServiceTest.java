package uk.co.nstauthority.scap.feedback;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.time.Clock;
import java.time.Instant;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;
import uk.co.fivium.feedbackmanagementservice.client.CannotSendFeedbackException;
import uk.co.fivium.feedbackmanagementservice.client.FeedbackClientService;
import uk.co.nstauthority.scap.TestEntityProvider;
import uk.co.nstauthority.scap.authentication.ServiceUserDetail;
import uk.co.nstauthority.scap.authentication.TestUserProvider;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.summary.ScapSummaryController;

@ExtendWith(MockitoExtension.class)
@DirtiesContext
class FeedbackServiceTest {

  private static final ServiceUserDetail LOGGED_IN_USER = TestUserProvider.getUser();
  private static final ScapDetail SCAP_DETAIL = TestEntityProvider.getScapDetail();
  private static final Instant CURRENT_INSTANT = Instant.now();
  private static final String CONTEXT_PATH = "test";
  private static final String SCAP_URL = "http://localhost/" + CONTEXT_PATH +
      ReverseRouter.route(on(ScapSummaryController.class).getScapSummary(SCAP_DETAIL.getScap().getScapId()));

  @Mock
  private Clock clock;

  @Mock
  private ScapDetailService scapDetailService;

  @Mock
  private FeedbackClientService feedbackClientService;

  @InjectMocks
  private FeedbackService feedbackService;

  @Captor
  private ArgumentCaptor<Feedback> feedbackArgumentCaptor;

  private FeedbackForm form;

  @BeforeAll
  static void setupStatic() {
    var request = new MockHttpServletRequest();
    request.setContextPath(CONTEXT_PATH);
    RequestContextHolder.setRequestAttributes(new ServletRequestAttributes(request));
  }

  @BeforeEach
  void setup() {
    form = new FeedbackForm();
    form.setSatisfactionRating(SatisfactionRating.NEITHER);
    form.setComments("test comments");

    doReturn(CURRENT_INSTANT).when(clock).instant();
  }

  @Test
  void saveFeedback_CannotSendFeedbackException_AssertDoesNotThrow() throws CannotSendFeedbackException {
    when(feedbackClientService.saveFeedback(any(Feedback.class)))
        .thenThrow(new CannotSendFeedbackException("test exception"));

    assertDoesNotThrow(() -> feedbackService.saveFeedback(null, form, LOGGED_IN_USER));

    verify(feedbackClientService).saveFeedback(feedbackArgumentCaptor.capture());

    assertThat(feedbackArgumentCaptor.getValue()).extracting(
        Feedback::getSubmitterName,
        Feedback::getSubmitterEmail,
        Feedback::getServiceRating,
        Feedback::getComment,
        Feedback::getGivenDatetime,
        Feedback::getTransactionId,
        Feedback::getTransactionReference,
        Feedback::getTransactionLink
    ).containsExactly(
        LOGGED_IN_USER.displayName(),
        LOGGED_IN_USER.emailAddress(),
        form.getSatisfactionRating().getEnumName(),
        form.getComments().getInputValue(),
        CURRENT_INSTANT,
        null,
        null,
        null
    );
  }

  @Test
  void saveFeedback_WithScapId_AssertDoesNotThrow() throws CannotSendFeedbackException {
    when(scapDetailService.getLatestByScapId(SCAP_DETAIL.getScap().getScapId())).thenReturn(SCAP_DETAIL);

    assertDoesNotThrow(() -> feedbackService.saveFeedback(SCAP_DETAIL.getScap().getScapId(), form, LOGGED_IN_USER));

    verify(feedbackClientService).saveFeedback(feedbackArgumentCaptor.capture());

    assertThat(feedbackArgumentCaptor.getValue()).extracting(
        Feedback::getSubmitterName,
        Feedback::getSubmitterEmail,
        Feedback::getServiceRating,
        Feedback::getComment,
        Feedback::getGivenDatetime,
        Feedback::getTransactionId,
        Feedback::getTransactionReference,
        Feedback::getTransactionLink
    ).containsExactly(
        LOGGED_IN_USER.displayName(),
        LOGGED_IN_USER.emailAddress(),
        form.getSatisfactionRating().getEnumName(),
        form.getComments().getInputValue(),
        CURRENT_INSTANT,
        String.valueOf(SCAP_DETAIL.getScap().getId()),
        SCAP_DETAIL.getScap().getReference(),
        SCAP_URL
    );
  }

}
