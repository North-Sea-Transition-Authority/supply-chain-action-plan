package uk.co.nstauthority.scap.scap.contractingperformance.summary;

import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.math.BigDecimal;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.RemunerationModel;
import uk.co.nstauthority.scap.scap.tasklist.TaskListController;

@ExtendWith(MockitoExtension.class)
class ContractingPerformanceSummaryViewTest {

  private ContractingPerformanceSummaryView contractingPerformanceSummaryView;

  @BeforeEach
  void setup() {
    contractingPerformanceSummaryView = new ContractingPerformanceSummaryView(
        1813,
        "some scope title",
        "some scope description",
        BigDecimal.valueOf(13),
        RemunerationModel.OTHER,
        "Some other remuneration model",
        "contractors name",
        0,
        BigDecimal.valueOf(14),
        "Something went wrong"
    );
  }

  @Test
  void getChangeLinkUrl() {
    var expectedChangeLinkUrl = ReverseRouter.route(on(TaskListController.class).renderTaskList(0));

    assertThat(contractingPerformanceSummaryView.getChangeLinkUrl()).isEqualTo(expectedChangeLinkUrl);
  }

  @Test
  void getDeleteLinkUrl() {
    var expectedDeleteLinkUrl = ReverseRouter.route(on(TaskListController.class).renderTaskList(0));

    assertThat(contractingPerformanceSummaryView.getDeleteLinkUrl()).isEqualTo(expectedDeleteLinkUrl);
  }
}
