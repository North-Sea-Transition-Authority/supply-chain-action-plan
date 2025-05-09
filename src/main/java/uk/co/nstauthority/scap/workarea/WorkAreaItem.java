package uk.co.nstauthority.scap.workarea;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.summary.ScapSubmissionStage;
import uk.co.nstauthority.scap.scap.summary.ScapSummaryController;

public record WorkAreaItem(ScapId scapId,
                           Integer scapVersion,
                           String reference,
                           String operator,
                           String projectName,
                           ScapDetailStatus status,
                           ScapSubmissionStage submissionStage,
                           Boolean outstandingInformationRequest,
                           Boolean updateInProgress,
                           String requestDueBy) {

  public String url() {
    return ReverseRouter.route(on(ScapSummaryController.class).getScapSummary(scapId));
  }
}
