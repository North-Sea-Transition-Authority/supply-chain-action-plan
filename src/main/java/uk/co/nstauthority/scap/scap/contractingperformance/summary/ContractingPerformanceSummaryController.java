package uk.co.nstauthority.scap.scap.contractingperformance.summary;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.List;
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
import uk.co.nstauthority.scap.endpointvalidation.annotations.ScapHasStatus;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.permissionmanagement.endpointsecurity.PermissionsRequiredForScap;
import uk.co.nstauthority.scap.scap.contractingperformance.ContractingPerformanceController;
import uk.co.nstauthority.scap.scap.contractingperformance.ContractingPerformanceOverviewService;
import uk.co.nstauthority.scap.scap.contractingperformance.hascontractingperformance.HasContractingPerformanceController;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.scap.ScapService;
import uk.co.nstauthority.scap.scap.summary.contractingperformance.ContractingPerformanceSummaryView;
import uk.co.nstauthority.scap.scap.summary.contractingperformance.ContractingPerformanceSummaryViewService;
import uk.co.nstauthority.scap.scap.tasklist.TaskListController;

@Controller
@RequestMapping("{scapId}/contracting-performance/summary")
@PermissionsRequiredForScap(permissions = RolePermission.SUBMIT_SCAP)
@ScapHasStatus(permittedStatuses = ScapDetailStatus.DRAFT)
public class ContractingPerformanceSummaryController {

  private final ContractingPerformanceSummaryViewService contractingPerformanceSummaryViewService;
  private final ScapService scapService;
  private final ScapDetailService scapDetailService;
  private final ContractingPerformanceOverviewService contractingPerformanceOverviewService;
  private final HasMoreContractingPerformanceFormService hasMoreContractingPerformanceFormService;
  private final ControllerHelperService controllerHelperService;

  @Autowired
  public ContractingPerformanceSummaryController(
      ContractingPerformanceSummaryViewService contractingPerformanceSummaryViewService,
      ScapService scapService, ScapDetailService scapDetailService,
      ContractingPerformanceOverviewService contractingPerformanceOverviewService,
      HasMoreContractingPerformanceFormService hasMoreContractingPerformanceFormService,
      ControllerHelperService controllerHelperService) {
    this.contractingPerformanceSummaryViewService = contractingPerformanceSummaryViewService;
    this.scapService = scapService;
    this.scapDetailService = scapDetailService;
    this.contractingPerformanceOverviewService = contractingPerformanceOverviewService;
    this.hasMoreContractingPerformanceFormService = hasMoreContractingPerformanceFormService;
    this.controllerHelperService = controllerHelperService;
  }

  @GetMapping
  public ModelAndView renderContractingPerformanceSummary(@PathVariable("scapId") ScapId scapId) {
    var scap = scapService.getScapById(scapId);
    var scapDetail = scapDetailService.getLatestByScap(scap);
    var contractingPerformanceOverview = contractingPerformanceOverviewService.getByScapDetailOrThrow(scapDetail);

    var summaryViews = contractingPerformanceSummaryViewService.getContractingPerformanceSummaryViews(scapId);
    if (summaryViews.isEmpty()) {
      return ReverseRouter.redirect(on(HasContractingPerformanceController.class)
          .renderHasContractingPerformanceForm(scapId));
    }
    var form = hasMoreContractingPerformanceFormService.getForm(contractingPerformanceOverview);
    return contractingPerformanceSummaryModelAndView(scapId, summaryViews)
        .addObject("form", form);
  }

  @PostMapping
  public ModelAndView saveContractingPerformanceSummary(@PathVariable("scapId") ScapId scapId,
                                                        @ModelAttribute("form") HasMoreContractingPerformanceForm form,
                                                        BindingResult bindingResult) {
    var scap = scapService.getScapById(scapId);
    var scapDetail = scapDetailService.getLatestByScap(scap);
    var contractingPerformanceOverview = contractingPerformanceOverviewService.getByScapDetailOrThrow(scapDetail);

    var summaryViews = contractingPerformanceSummaryViewService.getContractingPerformanceSummaryViews(scapId);
    if (summaryViews.isEmpty()) {
      return ReverseRouter.redirect(on(HasContractingPerformanceController.class)
          .renderHasContractingPerformanceForm(scapId));
    }

    bindingResult = hasMoreContractingPerformanceFormService.validate(form, bindingResult);

    return controllerHelperService.checkErrorsAndRedirect(
        bindingResult,
        contractingPerformanceSummaryModelAndView(scapId, summaryViews),
        form,
        () -> {
          if (HasMoreContractingPerformance.YES_NOW.equals(form.getHasMoreContractingPerformance())) {
            return ReverseRouter.redirect(on(ContractingPerformanceController.class)
                .renderNewContractingPerformanceForm(scapId, null));
          }
          contractingPerformanceOverviewService.updateHasMoreContractingPerformance(
              contractingPerformanceOverview, form.getHasMoreContractingPerformance());
          return ReverseRouter.redirect(on(TaskListController.class).renderTaskList(scapId));
        }
    );
  }

  private ModelAndView contractingPerformanceSummaryModelAndView(ScapId scapId,
                                                                 List<ContractingPerformanceSummaryView> summaryViews) {
    return new ModelAndView("scap/scap/contractingperformance/contractingPerformanceSummary")
        .addObject("backLinkUrl", ReverseRouter.route(on(TaskListController.class).renderTaskList(scapId)))
        .addObject("summaryViews", summaryViews)
        .addObject("radioItems", HasMoreContractingPerformance.getRadioItems());
  }
}
