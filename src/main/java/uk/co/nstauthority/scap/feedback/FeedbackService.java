package uk.co.nstauthority.scap.feedback;


import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.time.Clock;
import java.util.Objects;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.fivium.feedbackmanagementservice.client.CannotSendFeedbackException;
import uk.co.fivium.feedbackmanagementservice.client.FeedbackClientService;
import uk.co.nstauthority.scap.authentication.ServiceUserDetail;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.summary.ScapSummaryController;
import uk.co.nstauthority.scap.util.AbsoluteReverseRouter;

@Service
class FeedbackService {

  private static final Logger LOGGER = LoggerFactory.getLogger(FeedbackService.class);

  private final Clock clock;
  private final ScapDetailService scapDetailService;
  private final FeedbackClientService feedbackClientService;

  @Autowired
  FeedbackService(Clock clock,
                  ScapDetailService scapDetailService,
                  FeedbackClientService feedbackClientService) {
    this.clock = clock;
    this.scapDetailService = scapDetailService;
    this.feedbackClientService = feedbackClientService;
  }

  public void saveFeedback(ScapId scapId,
                           FeedbackForm form,
                           ServiceUserDetail userDetails) {
    var builder = getBaseFeedbackBuilder(form, userDetails);

    if (Objects.nonNull(scapId)) {
      var scapDetail = scapDetailService.getLatestByScapId(scapId);
      builder.transactionReference(scapDetail.getScap().getReference());
      builder.transactionLink(
          AbsoluteReverseRouter.route(on(ScapSummaryController.class).getScapSummary(scapId)));
      builder.transactionId(String.valueOf(scapId.scapId()));
    }

    submitFeedback(builder.build());
  }

  private void submitFeedback(Feedback feedback) {
    try {
      feedbackClientService.saveFeedback(feedback);
    } catch (CannotSendFeedbackException e) {
      LOGGER.error("Failed to submit feedback to FMS. %s".formatted(feedback), e);
    }
  }


  private Feedback.Builder getBaseFeedbackBuilder(FeedbackForm form, ServiceUserDetail userDetails) {
    return Feedback.builder()
        .userDetails(userDetails)
        .timestamp(clock.instant())
        .serviceRating(form.getSatisfactionRating())
        .comment(form.getComments().getInputValue());
  }
}
