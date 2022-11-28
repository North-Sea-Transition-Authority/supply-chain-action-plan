package uk.co.nstauthority.scap.scap.contractingperformance.summary;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.List;
import java.util.Map;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.contractingperformance.hascontractingperformance.HasContractingPerformanceController;
import uk.co.nstauthority.scap.scap.tasklist.TaskListController;

@Controller
@RequestMapping("{scapId}/contracting-performance/summary")
public class ContractingPerformanceSummaryController {

  private final ContractingPerformanceSummaryService contractingPerformanceSummaryService;

  @Autowired
  public ContractingPerformanceSummaryController(
      ContractingPerformanceSummaryService contractingPerformanceSummaryService) {
    this.contractingPerformanceSummaryService = contractingPerformanceSummaryService;
  }

  @GetMapping
  public ModelAndView renderContractingPerformanceSummary(@PathVariable("scapId") Integer scapId) {
    var summaryViews = contractingPerformanceSummaryService.getContractingPerformanceSummaryViews(scapId);
    if (summaryViews.isEmpty()) {
      return ReverseRouter.redirect(on(HasContractingPerformanceController.class)
          .renderHasContractingPerformanceForm(scapId));
    }
    var countryMap = contractingPerformanceSummaryService.getCountryMap(summaryViews);
    return contractingPerformanceSummaryModelAndView(scapId, summaryViews, countryMap);
  }

  private ModelAndView contractingPerformanceSummaryModelAndView(Integer scapId,
                                                                 List<ContractingPerformanceSummaryView> summaryViews,
                                                                 Map<String, String> countryMap) {
    return new ModelAndView("scap/scap/contractingperformance/contractingPerformanceSummary")
        .addObject("backLinkUrl", ReverseRouter.route(on(TaskListController.class).renderTaskList(scapId)))
        .addObject("summaryViews", summaryViews)
        .addObject("countryMap", countryMap);
  }
}
