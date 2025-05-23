package uk.co.nstauthority.scap.scap.contractingperformance.hascontractingperformance;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;
import uk.co.nstauthority.scap.controllerhelper.ControllerHelperService;
import uk.co.nstauthority.scap.endpointvalidation.annotations.HasAnyPermissionForScap;
import uk.co.nstauthority.scap.endpointvalidation.annotations.ScapHasStatus;
import uk.co.nstauthority.scap.enumutil.YesNo;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.scap.contractingperformance.ContractingPerformanceController;
import uk.co.nstauthority.scap.scap.contractingperformance.ContractingPerformanceOverviewService;
import uk.co.nstauthority.scap.scap.contractingperformance.summary.ContractingPerformanceSummaryController;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.scap.ScapService;
import uk.co.nstauthority.scap.scap.summary.contractingperformance.ContractingPerformanceSummaryViewService;
import uk.co.nstauthority.scap.scap.tasklist.TaskListController;

@Controller
@RequestMapping("{scapId}/contracting-performance")
@HasAnyPermissionForScap(permissions = RolePermission.SUBMIT_SCAP)
@ScapHasStatus(permittedStatuses = ScapDetailStatus.DRAFT)
public class HasContractingPerformanceController {

  private final ScapService scapService;
  private final ScapDetailService scapDetailService;
  private final ContractingPerformanceOverviewService contractingPerformanceOverviewService;
  private final HasContractingPerformanceFormService contractingPerformanceFormService;
  private final ControllerHelperService controllerHelperService;
  private final ContractingPerformanceSummaryViewService contractingPerformanceSummaryViewService;

  @Autowired
  public HasContractingPerformanceController(ScapService scapService, ScapDetailService scapDetailService,
                                             ContractingPerformanceOverviewService contractingPerformanceOverviewService,
                                             HasContractingPerformanceFormService contractingPerformanceFormService,
                                             ControllerHelperService controllerHelperService,
                                             ContractingPerformanceSummaryViewService contractingPerformanceSummaryViewService) {
    this.scapService = scapService;
    this.scapDetailService = scapDetailService;
    this.contractingPerformanceOverviewService = contractingPerformanceOverviewService;
    this.contractingPerformanceFormService = contractingPerformanceFormService;
    this.controllerHelperService = controllerHelperService;
    this.contractingPerformanceSummaryViewService = contractingPerformanceSummaryViewService;
  }

  @GetMapping
  public ModelAndView renderHasContractingPerformanceForm(@PathVariable("scapId") ScapId scapId) {
    var scap = scapService.getScapById(scapId);
    var scapDetail = scapDetailService.getLatestByScap(scap);
    var contractingPerformanceOverview = contractingPerformanceOverviewService.findByScapDetail(scapDetail);
    var summaryViews = contractingPerformanceSummaryViewService.getContractingPerformanceSummaryViews(scapId);

    if (!summaryViews.isEmpty()) {
      return ReverseRouter.redirect(on(ContractingPerformanceSummaryController.class)
          .renderContractingPerformanceSummary(scapId));
    }

    var form = contractingPerformanceOverview.map(contractingPerformanceFormService::getForm)
        .orElse(new HasContractingPerformanceForm());

    return hasContractingPerformanceModelAndView(scapId)
        .addObject("form", form);
  }

  @PostMapping
  ModelAndView saveHasContractingPerformanceForm(@PathVariable("scapId") ScapId scapId,
                                                 @ModelAttribute("form") HasContractingPerformanceForm form,
                                                 BindingResult bindingResult) {

    var scap = scapService.getScapById(scapId);
    var scapDetail = scapDetailService.getLatestByScap(scap);
    var summaryViews = contractingPerformanceSummaryViewService.getContractingPerformanceSummaryViews(scapId);

    if (!summaryViews.isEmpty()) {
      return ReverseRouter.redirect(on(ContractingPerformanceSummaryController.class)
          .renderContractingPerformanceSummary(scapId));
    }

    bindingResult = contractingPerformanceFormService.validate(form, bindingResult);

    return controllerHelperService.checkErrorsAndRedirect(
        bindingResult,
        hasContractingPerformanceModelAndView(scapId),
        form,
        () -> {
          contractingPerformanceOverviewService.saveContractingPerformance(scapDetail, form.getHasContractingPerformance());
          if (YesNo.NO.equals(form.getHasContractingPerformance())) {
            return ReverseRouter.redirect(on(TaskListController.class).renderTaskList(scapId));
          }
          return ReverseRouter.redirect(on(ContractingPerformanceController.class)
              .renderNewContractingPerformanceForm(scapId, null));
        }
    );
  }

  private ModelAndView hasContractingPerformanceModelAndView(ScapId scapId) {
    return new ModelAndView("scap/scap/contractingperformance/hasContractingPerformance")
        .addObject("backLinkUrl", ReverseRouter.route(on(TaskListController.class).renderTaskList(scapId)))
        .addObject("radioItems", YesNo.getRadioOptions());
  }
}
