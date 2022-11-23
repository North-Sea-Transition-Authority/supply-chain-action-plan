package uk.co.nstauthority.scap.scap.contractingperformance;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.Collections;
import java.util.Map;
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
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.actualtender.ActualTenderService;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivityService;
import uk.co.nstauthority.scap.scap.contractingperformance.hascontractingperformance.HasContractingPerformanceController;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.scap.ScapService;
import uk.co.nstauthority.scap.scap.tasklist.TaskListController;

@Controller
@RequestMapping("{scapId}/contracting-performance")
public class ContractingPerformanceController {

  private final ScapService scapService;
  private final ScapDetailService scapDetailService;
  private final ContractingPerformanceOverviewService contractingPerformanceOverviewService;
  private final ContractingPerformanceService contractingPerformanceService;
  private final ContractingPerformanceFormService contractingPerformanceFormService;
  private final ControllerHelperService controllerHelperService;
  private final ActualTenderService actualTenderService;
  private final ActualTenderActivityService actualTenderActivityService;

  @Autowired
  ContractingPerformanceController(ScapService scapService, ScapDetailService scapDetailService,
                                   ContractingPerformanceOverviewService contractingPerformanceOverviewService,
                                   ContractingPerformanceService contractingPerformanceService,
                                   ContractingPerformanceFormService contractingPerformanceFormService,
                                   ControllerHelperService controllerHelperService,
                                   ActualTenderService actualTenderService,
                                   ActualTenderActivityService actualTenderActivityService) {
    this.scapService = scapService;
    this.scapDetailService = scapDetailService;
    this.contractingPerformanceOverviewService = contractingPerformanceOverviewService;
    this.contractingPerformanceService = contractingPerformanceService;
    this.contractingPerformanceFormService = contractingPerformanceFormService;
    this.controllerHelperService = controllerHelperService;
    this.actualTenderService = actualTenderService;
    this.actualTenderActivityService = actualTenderActivityService;
  }

  @GetMapping("/new")
  public ModelAndView renderNewContractingPerformanceForm(@PathVariable("scapId") Integer scapId,
                                                          @ModelAttribute("form") ContractingPerformanceForm form) {

    var scap = scapService.getScapById(scapId);
    var scapDetail = scapDetailService.getLatestScapDetailByScapOrThrow(scap);
    contractingPerformanceOverviewService.getByScapDetailOrThrow(scapDetail);
    var actualTender = actualTenderService.getByScapDetail(scapDetail);
    var contractedActivities = actualTender
        .map(actualTenderActivityService::getActivitiesWithContractAwarded)
        .orElse(Collections.emptyList());
    var scopeTitlesMap = contractingPerformanceFormService.getScopeTitlesMap(contractedActivities);
    return contractingPerformanceFormModelAndView(scapId, scopeTitlesMap);
  }

  @PostMapping("/new")
  public ModelAndView saveNewContractingPerformanceForm(@PathVariable("scapId") Integer scapId,
                                                        @ModelAttribute("form") ContractingPerformanceForm form,
                                                        BindingResult bindingResult) {
    var scap = scapService.getScapById(scapId);
    var scapDetail = scapDetailService.getLatestScapDetailByScapOrThrow(scap);
    var contractingPerformanceOverview = contractingPerformanceOverviewService.getByScapDetailOrThrow(scapDetail);
    var actualTender = actualTenderService.getByScapDetail(scapDetail);
    var contractedActivities = actualTender
        .map(actualTenderActivityService::getActivitiesWithContractAwarded)
        .orElse(Collections.emptyList());
    var scopeTitlesMap = contractingPerformanceFormService.getScopeTitlesMap(contractedActivities);

    bindingResult = contractingPerformanceFormService.validate(form, bindingResult, contractedActivities);

    return controllerHelperService.checkErrorsAndRedirect(
        bindingResult,
        contractingPerformanceFormModelAndView(scapId, scopeTitlesMap),
        form,
        () -> {
          contractingPerformanceService.createContractingPerformance(contractingPerformanceOverview, form);
          return ReverseRouter.redirect(on(TaskListController.class).renderTaskList(scapId));
        }
    );
  }

  private ModelAndView contractingPerformanceFormModelAndView(Integer scapId, Map<String, String> scopeTitlesMap) {
    return new ModelAndView("scap/scap/contractingperformance/contractingPerformanceDetail")
        // TODO: SCAP2022-50 if there are existing contracting performance details, redirect to contracting performance summary
        .addObject("backLinkUrl",
            ReverseRouter.route(on(HasContractingPerformanceController.class)
                .renderHasContractingPerformanceForm(scapId)))
        .addObject("scopeTitlesMap", scopeTitlesMap);
  }

}
