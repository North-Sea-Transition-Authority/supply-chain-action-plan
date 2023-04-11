package uk.co.nstauthority.scap.scap.contractingperformance.delete;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;
import uk.co.nstauthority.scap.endpointvalidation.annotations.ScapHasStatus;
import uk.co.nstauthority.scap.error.exception.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.permissionmanagement.endpointsecurity.PermissionsRequiredForScap;
import uk.co.nstauthority.scap.scap.contractingperformance.ContractingPerformanceOverviewService;
import uk.co.nstauthority.scap.scap.contractingperformance.ContractingPerformanceService;
import uk.co.nstauthority.scap.scap.contractingperformance.hascontractingperformance.HasContractingPerformanceController;
import uk.co.nstauthority.scap.scap.contractingperformance.summary.ContractingPerformanceSummaryController;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.scap.ScapService;
import uk.co.nstauthority.scap.scap.summary.contractingperformance.ContractingPerformanceSummaryViewService;
import uk.co.nstauthority.scap.util.SuccessBannerUtil;

@Controller
@RequestMapping("{scapId}/contracting-performance/{contractingPerformanceId}/delete")
@PermissionsRequiredForScap(permissions = RolePermission.SUBMIT_SCAP)
@ScapHasStatus(permittedStatuses = ScapDetailStatus.DRAFT)
public class DeleteContractingPerformanceController {

  private final ScapService scapService;
  private final ScapDetailService scapDetailService;
  private final ContractingPerformanceOverviewService contractingPerformanceOverviewService;
  private final ContractingPerformanceService contractingPerformanceService;
  private final ContractingPerformanceSummaryViewService contractingPerformanceSummaryViewService;

  @Autowired
  DeleteContractingPerformanceController(ScapService scapService,
                                         ScapDetailService scapDetailService,
                                         ContractingPerformanceOverviewService contractingPerformanceOverviewService,
                                         ContractingPerformanceService contractingPerformanceService,
                                         ContractingPerformanceSummaryViewService contractingPerformanceSummaryViewService) {
    this.scapService = scapService;
    this.scapDetailService = scapDetailService;
    this.contractingPerformanceOverviewService = contractingPerformanceOverviewService;
    this.contractingPerformanceService = contractingPerformanceService;
    this.contractingPerformanceSummaryViewService = contractingPerformanceSummaryViewService;
  }

  @GetMapping
  public ModelAndView renderDeleteContractingPerformanceConfirmation(@PathVariable("scapId") ScapId scapId,
                                                                     @PathVariable("contractingPerformanceId")
                                                                     Integer contractingPerformanceId) {
    var summaryView = contractingPerformanceSummaryViewService
        .getContractingPerformanceSummaryView(scapId, contractingPerformanceId)
        .orElseThrow(() -> new ScapEntityNotFoundException(
            "Could not get ContractingPerformanceSummaryDto for scapId: [%d] and contractingPerformanceId: [%d]"
                .formatted(scapId.scapId(), contractingPerformanceId)));

    return new ModelAndView("scap/scap/contractingperformance/deleteContractingPerformance")
        .addObject("backLinkUrl", ReverseRouter.route(on(ContractingPerformanceSummaryController.class)
            .renderContractingPerformanceSummary(scapId)))
        .addObject("summaryView", summaryView);
  }

  @PostMapping
  public ModelAndView saveDeleteContractingPerformance(@PathVariable("scapId") ScapId scapId,
                                                       @PathVariable("contractingPerformanceId")
                                                       Integer contractingPerformanceId,
                                                       RedirectAttributes redirectAttributes) {
    var scap = scapService.getScapById(scapId);
    var scapDetail = scapDetailService.getLatestScapDetailByScapOrThrow(scap);
    var contractingPerformanceOverview = contractingPerformanceOverviewService
        .getByScapDetailOrThrow(scapDetail);

    var contractingPerformance = contractingPerformanceService.getById(contractingPerformanceId);
    contractingPerformanceService.deleteContractingPerformance(contractingPerformance);
    SuccessBannerUtil.add(redirectAttributes,
        "Deleted contracting performance for \"%s\" successfully"
            .formatted(contractingPerformance.getActualTenderActivity().getScopeTitle()));

    if (contractingPerformanceService.hasContractingPerformance(contractingPerformanceOverview)) {
      return ReverseRouter.redirect(on(ContractingPerformanceSummaryController.class)
          .renderContractingPerformanceSummary(scapId));
    }

    contractingPerformanceOverviewService.updateHasMoreContractingPerformance(contractingPerformanceOverview, null);
    return ReverseRouter.redirect(on(HasContractingPerformanceController.class)
        .renderHasContractingPerformanceForm(scapId));
  }

}
