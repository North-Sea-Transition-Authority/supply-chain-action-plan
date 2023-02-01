package uk.co.nstauthority.scap.workarea;

import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.summary.ScapSubmissionStage;
import uk.co.nstauthority.scap.scap.summary.ScapSummaryController;
import uk.co.nstauthority.scap.scap.tasklist.TaskListController;

@ExtendWith(SpringExtension.class)
class WorkAreaItemTest {

  private static final ScapId SCAP_ID = new ScapId(1);

  @Test
  void url_WhenFirstDraft_AssertTaskListUrl() {
    var workAreaItem = new WorkAreaItem(
        SCAP_ID,
        1,
        "SCAP/2023/1",
        "CENTRICA",
        "Some project name",
        ScapDetailStatus.DRAFT,
        ScapSubmissionStage.CONTRACTING_STRATEGY_PENDING,
        false
    );
    var expectedUrl = ReverseRouter.route(on(TaskListController.class).renderTaskList(workAreaItem.scapId()));

    assertThat(workAreaItem.url()).isEqualTo(expectedUrl);
  }

  @Test
  void url_WhenNotFirstDraft_AssertSummaryUrl() {
    var workAreaItem = new WorkAreaItem(
        SCAP_ID,
        2,
        "SCAP/2023/1",
        "CENTRICA",
        "Some project name",
        ScapDetailStatus.DRAFT,
        ScapSubmissionStage.CONTRACTING_STRATEGY_PENDING,
        false
    );
    var expectedUrl = ReverseRouter.route(on(ScapSummaryController.class)
        .getScapSummary(workAreaItem.scapId()));

    assertThat(workAreaItem.url()).isEqualTo(expectedUrl);
  }

  @Test
  void url_WhenFirstSubmission_AssertSummaryUrl() {
    var workAreaItem = new WorkAreaItem(
        SCAP_ID,
        1,
        "SCAP/2023/1",
        "CENTRICA",
        "Some project name",
        ScapDetailStatus.SUBMITTED,
        ScapSubmissionStage.CONTRACTING_STRATEGY_PENDING,
        false
    );
    var expectedUrl = ReverseRouter.route(on(ScapSummaryController.class)
        .getScapSummary(workAreaItem.scapId()));

    assertThat(workAreaItem.url()).isEqualTo(expectedUrl);
  }

  @Test
  void url_WhenNotFirstSubmission_AssertSummaryUrl() {
    var workAreaItem = new WorkAreaItem(
        SCAP_ID,
        2,
        "SCAP/2023/1",
        "CENTRICA",
        "Some project name",
        ScapDetailStatus.SUBMITTED,
        ScapSubmissionStage.CONTRACTING_STRATEGY_PENDING,
        false
    );
    var expectedUrl = ReverseRouter.route(on(ScapSummaryController.class)
        .getScapSummary(workAreaItem.scapId()));

    assertThat(workAreaItem.url()).isEqualTo(expectedUrl);
  }
}
