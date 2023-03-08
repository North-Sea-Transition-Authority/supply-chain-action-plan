package uk.co.nstauthority.scap.workarea;

import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.summary.ScapSubmissionStage;
import uk.co.nstauthority.scap.scap.summary.ScapSummaryController;

@ExtendWith(SpringExtension.class)
class WorkAreaItemTest {

  private static final ScapId SCAP_ID = new ScapId(1);

  @Test
  void url() {
    var workAreaItem = new WorkAreaItem(
        SCAP_ID,
        1,
        "SCAP/2023/1",
        "CENTRICA",
        "Some project name",
        ScapDetailStatus.DRAFT,
        ScapSubmissionStage.CONTRACTING_STRATEGY_PENDING,
        false,
        false,
        null
    );
    var expectedUrl = ReverseRouter.route(on(ScapSummaryController.class).getScapSummary(workAreaItem.scapId()));

    assertThat(workAreaItem.url()).isEqualTo(expectedUrl);
  }
}
