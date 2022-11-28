package uk.co.nstauthority.scap.scap.contractingperformance.summary;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.math.BigDecimal;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.RemunerationModel;
import uk.co.nstauthority.scap.scap.tasklist.TaskListController;

public record ContractingPerformanceSummaryView(Integer contractingPerformanceId,
                                                String scopeTitle,
                                                String scopeDescription,
                                                BigDecimal awardValue,
                                                RemunerationModel remunerationModel,
                                                String remunerationModelName,
                                                String contractor,
                                                Integer countryId,
                                                BigDecimal outturnCost,
                                                String outturnRationale) {

  public String getChangeLinkUrl() {
    // TODO: SCAP2022-52 - Replace with link to update contracting performance (using contractingPerformanceId)
    return ReverseRouter.route(on(TaskListController.class).renderTaskList(0));
  }

  public String getDeleteLinkUrl() {
    // TODO: SCAP2022-51 - Replace with link to delete contracting performance (using contractingPerformanceId)
    return ReverseRouter.route(on(TaskListController.class).renderTaskList(0));
  }

}
