package uk.co.nstauthority.scap.scap.contractingperformance.delete;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.List;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;
import uk.co.nstauthority.scap.error.exception.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.contractingperformance.ContractingPerformanceOverviewService;
import uk.co.nstauthority.scap.scap.contractingperformance.ContractingPerformanceService;
import uk.co.nstauthority.scap.scap.contractingperformance.hascontractingperformance.HasContractingPerformanceController;
import uk.co.nstauthority.scap.scap.contractingperformance.summary.ContractingPerformanceSummaryController;
import uk.co.nstauthority.scap.scap.contractingperformance.summary.ContractingPerformanceSummaryService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.scap.ScapService;
import uk.co.nstauthority.scap.util.DeletionSuccessBannerUtil;

@Controller
@RequestMapping("{scapId}/contracting-performance/{contractingPerformanceId}/delete")
public class DeleteContractingPerformanceController {

  private final ScapService scapService;
  private final ScapDetailService scapDetailService;
  private final ContractingPerformanceOverviewService contractingPerformanceOverviewService;
  private final ContractingPerformanceService contractingPerformanceService;
  private final ContractingPerformanceSummaryService contractingPerformanceSummaryService;

  @Autowired
  DeleteContractingPerformanceController(ScapService scapService,
                                         ScapDetailService scapDetailService,
                                         ContractingPerformanceOverviewService contractingPerformanceOverviewService,
                                         ContractingPerformanceService contractingPerformanceService,
                                         ContractingPerformanceSummaryService contractingPerformanceSummaryService) {
    this.scapService = scapService;
    this.scapDetailService = scapDetailService;
    this.contractingPerformanceOverviewService = contractingPerformanceOverviewService;
    this.contractingPerformanceService = contractingPerformanceService;
    this.contractingPerformanceSummaryService = contractingPerformanceSummaryService;
  }

  @GetMapping
  public ModelAndView renderDeleteContractingPerformanceConfirmation(@PathVariable("scapId") Integer scapId,
                                                                     @PathVariable("contractingPerformanceId")
                                                                     Integer contractingPerformanceId) {
    var summaryView = contractingPerformanceSummaryService.getContractingPerformanceSummaryView(scapId, contractingPerformanceId)
        .orElseThrow(() -> new ScapEntityNotFoundException(
            "Could not get ContractingPerformanceSummaryView for scapId: [%d] and contractingPerformanceId: [%d]"
                .formatted(scapId, contractingPerformanceId)));
    var countryMap = contractingPerformanceSummaryService.getCountryMap(List.of(summaryView));

    return new ModelAndView("scap/scap/contractingperformance/deleteContractingPerformance")
        .addObject("backLinkUrl", ReverseRouter.route(on(ContractingPerformanceSummaryController.class)
            .renderContractingPerformanceSummary(scapId)))
        .addObject("summaryView", summaryView)
        .addObject("countryMap", countryMap);
  }

  @PostMapping
  public ModelAndView saveDeleteContractingPerformance(@PathVariable("scapId") Integer scapId,
                                                       @PathVariable("contractingPerformanceId")
                                                       Integer contractingPerformanceId,
                                                       RedirectAttributes redirectAttributes) {
    var scap = scapService.getScapById(scapId);
    var scapDetail = scapDetailService.getLatestScapDetailByScapOrThrow(scap);
    var contractingPerformanceOverview = contractingPerformanceOverviewService
        .getByScapDetailOrThrow(scapDetail);

    var contractingPerformance = contractingPerformanceService.getById(contractingPerformanceId);
    contractingPerformanceService.deleteContractingPerformance(contractingPerformance);
    DeletionSuccessBannerUtil.addRedirectionNotification(redirectAttributes,
        "Deleted contracting performance for \"%s\" successfully"
            .formatted(contractingPerformance.getActualTenderActivity().getScopeTitle()));

    if (contractingPerformanceService.hasContractingPerformance(contractingPerformanceOverview)) {
      return ReverseRouter.redirect(on(ContractingPerformanceSummaryController.class)
          .renderContractingPerformanceSummary(scapId));
    }

    return ReverseRouter.redirect(on(HasContractingPerformanceController.class)
        .renderHasContractingPerformanceForm(scapId));
  }

}
