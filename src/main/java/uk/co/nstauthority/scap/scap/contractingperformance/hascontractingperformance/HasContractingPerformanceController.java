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
import uk.co.nstauthority.scap.enumutil.YesNo;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.contractingperformance.ContractingPerformanceOverviewService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.scap.ScapService;
import uk.co.nstauthority.scap.scap.tasklist.TaskListController;

@Controller
@RequestMapping("{scapId}/contracting-performance")
public class HasContractingPerformanceController {

  private final ScapService scapService;
  private final ScapDetailService scapDetailService;
  private final ContractingPerformanceOverviewService contractingPerformanceOverviewService;
  private final HasContractingPerformanceFormService contractingPerformanceFormService;
  private final ControllerHelperService controllerHelperService;

  @Autowired
  public HasContractingPerformanceController(ScapService scapService, ScapDetailService scapDetailService,
                                             ContractingPerformanceOverviewService contractingPerformanceOverviewService,
                                             HasContractingPerformanceFormService contractingPerformanceFormService,
                                             ControllerHelperService controllerHelperService) {
    this.scapService = scapService;
    this.scapDetailService = scapDetailService;
    this.contractingPerformanceOverviewService = contractingPerformanceOverviewService;
    this.contractingPerformanceFormService = contractingPerformanceFormService;
    this.controllerHelperService = controllerHelperService;
  }

  @GetMapping
  public ModelAndView renderHasContractingPerformanceForm(@PathVariable("scapId") Integer scapId) {
    var scap = scapService.getScapById(scapId);
    var scapDetail = scapDetailService.getLatestScapDetailByScapOrThrow(scap);
    var contractingPerformanceOverview = contractingPerformanceOverviewService.getByScapDetail(scapDetail);
    var form = contractingPerformanceOverview.map(contractingPerformanceFormService::getForm)
        .orElse(new HasContractingPerformanceForm());

    return hasContractingPerformanceModelAndView(scapId)
        .addObject("form", form);
  }

  @PostMapping
  ModelAndView saveHasContractingPerformanceForm(@PathVariable("scapId") Integer scapId,
                                                 @ModelAttribute("form") HasContractingPerformanceForm form,
                                                 BindingResult bindingResult) {

    var scap = scapService.getScapById(scapId);
    var scapDetail = scapDetailService.getLatestScapDetailByScapOrThrow(scap);

    // TODO: SCAP2022-49 - In PR for next form page add check for if there are already contracting performances for this,
    // TODO: for now redirect to task list, replace this with TODO for SCAP2022-50

    bindingResult = contractingPerformanceFormService.validate(form, bindingResult);

    return controllerHelperService.checkErrorsAndRedirect(
        bindingResult,
        hasContractingPerformanceModelAndView(scapId),
        form,
        // TODO SCAP2022-49: In PR for next form page change this redirect
        () -> {
          contractingPerformanceOverviewService.saveContractingPerformance(scapDetail, form.getHasContractingPerformance());
          return ReverseRouter.redirect(on(TaskListController.class).renderTaskList(scapId));
        }
    );
  }

  private ModelAndView hasContractingPerformanceModelAndView(Integer scapId) {
    return new ModelAndView("scap/scap/contractingperformance/hasContractingPerformance")
        .addObject("backLinkUrl", ReverseRouter.route(on(TaskListController.class).renderTaskList(scapId)))
        .addObject("radioItems", YesNo.getRadioOptions());
  }
}
