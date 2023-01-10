package uk.co.nstauthority.scap.scap.contractingperformance;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.Collections;
import java.util.List;
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
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.permissionmanagement.endpointsecurity.PermissionsRequiredForScap;
import uk.co.nstauthority.scap.scap.actualtender.ActualTenderService;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivity;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivityService;
import uk.co.nstauthority.scap.scap.contractingperformance.hascontractingperformance.HasContractingPerformanceController;
import uk.co.nstauthority.scap.scap.contractingperformance.summary.ContractingPerformanceSummaryController;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.scap.ScapService;

@Controller
@RequestMapping("{scapId}/contracting-performance")
@PermissionsRequiredForScap(permissions = RolePermission.SUBMIT_SCAP)
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
  public ModelAndView renderNewContractingPerformanceForm(@PathVariable("scapId") ScapId scapId,
                                                          @ModelAttribute("form") ContractingPerformanceForm form) {

    var scap = scapService.getScapById(scapId);
    var scapDetail = scapDetailService.getLatestScapDetailByScapOrThrow(scap);
    var contractingPerformanceOverview = contractingPerformanceOverviewService.getByScapDetailOrThrow(scapDetail);
    var actualTender = actualTenderService.getByScapDetail(scapDetail);
    var contractedActivities = actualTender
        .map(actualTenderActivityService::getActivitiesWithContractAwarded)
        .orElse(Collections.emptyList());

    var scopeTitlesMap = getScopeTitlesMap(contractedActivities);

    return contractingPerformanceFormModelAndView(scopeTitlesMap,
        getBackLinkUrl(scapId, contractingPerformanceOverview));
  }

  @PostMapping("/new")
  public ModelAndView saveNewContractingPerformanceForm(@PathVariable("scapId") ScapId scapId,
                                                        @ModelAttribute("form") ContractingPerformanceForm form,
                                                        BindingResult bindingResult) {
    var scap = scapService.getScapById(scapId);
    var scapDetail = scapDetailService.getLatestScapDetailByScapOrThrow(scap);
    var contractingPerformanceOverview = contractingPerformanceOverviewService.getByScapDetailOrThrow(scapDetail);
    var actualTender = actualTenderService.getByScapDetail(scapDetail);
    var contractedActivities = actualTender
        .map(actualTenderActivityService::getActivitiesWithContractAwarded)
        .orElse(Collections.emptyList());

    var scopeTitlesMap = getScopeTitlesMap(contractedActivities);

    bindingResult = contractingPerformanceFormService.validate(form, bindingResult, contractedActivities);

    return controllerHelperService.checkErrorsAndRedirect(
        bindingResult,
        contractingPerformanceFormModelAndView(scopeTitlesMap,
            getBackLinkUrl(scapId, contractingPerformanceOverview)),
        form,
        () -> {
          contractingPerformanceService.createContractingPerformance(contractingPerformanceOverview, form);
          return ReverseRouter.redirect(on(ContractingPerformanceSummaryController.class)
              .renderContractingPerformanceSummary(scapId));
        }
    );
  }

  @GetMapping("/{contractingPerformanceId}")
  public ModelAndView renderExistingContractingPerformanceForm(@PathVariable("scapId") ScapId scapId,
                                                               @PathVariable("contractingPerformanceId")
                                                               Integer contractingPerformanceId) {
    var scap = scapService.getScapById(scapId);
    var scapDetail = scapDetailService.getLatestScapDetailByScapOrThrow(scap);
    var actualTender = actualTenderService.getByScapDetail(scapDetail);
    var contractedActivities = actualTender
        .map(actualTenderActivityService::getActivitiesWithContractAwarded)
        .orElse(Collections.emptyList());
    var contractingPerformance = contractingPerformanceService.getById(contractingPerformanceId);

    var form = contractingPerformanceFormService.getForm(contractingPerformance);

    var scopeTitlesMap = getScopeTitlesMapWithCurrent(contractedActivities, contractingPerformance);

    return contractingPerformanceFormModelAndView(scopeTitlesMap,
        ReverseRouter.route(on(ContractingPerformanceSummaryController.class)
            .renderContractingPerformanceSummary(scapId)))
        .addObject("form", form);
  }

  @PostMapping("/{contractingPerformanceId}")
  public ModelAndView saveExistingContractingPerformanceForm(@PathVariable("scapId") ScapId scapId,
                                                             @PathVariable("contractingPerformanceId")
                                                             Integer contractingPerformanceId,
                                                             @ModelAttribute("form") ContractingPerformanceForm form,
                                                             BindingResult bindingResult) {
    var scap = scapService.getScapById(scapId);
    var scapDetail = scapDetailService.getLatestScapDetailByScapOrThrow(scap);
    var actualTender = actualTenderService.getByScapDetail(scapDetail);
    var contractedActivities = actualTender
        .map(actualTenderActivityService::getActivitiesWithContractAwarded)
        .orElse(Collections.emptyList());
    var contractingPerformance = contractingPerformanceService.getById(contractingPerformanceId);

    var scopeTitlesMap = getScopeTitlesMapWithCurrent(contractedActivities, contractingPerformance);

    bindingResult = contractingPerformanceFormService.validate(form, bindingResult, contractedActivities);

    return controllerHelperService.checkErrorsAndRedirect(
        bindingResult,
        contractingPerformanceFormModelAndView(scopeTitlesMap,
            ReverseRouter.route(on(ContractingPerformanceSummaryController.class)
                .renderContractingPerformanceSummary(scapId))),
        form,
        () -> {
          contractingPerformanceService.saveContractingPerformance(contractingPerformance, form);
          return ReverseRouter.redirect(on(ContractingPerformanceSummaryController.class)
              .renderContractingPerformanceSummary(scapId));
        }
    );
  }

  private String getBackLinkUrl(ScapId scapId, ContractingPerformanceOverview contractingPerformanceOverview) {
    if (contractingPerformanceService.hasContractingPerformance(contractingPerformanceOverview)) {
      return ReverseRouter.route(on(ContractingPerformanceSummaryController.class)
          .renderContractingPerformanceSummary(scapId));
    }
    return ReverseRouter.route(on(HasContractingPerformanceController.class)
        .renderHasContractingPerformanceForm(scapId));
  }

  private Map<String, String> getScopeTitlesMap(List<ActualTenderActivity> contractedActivities) {
    var activitiesWithoutContractingPerformance = contractingPerformanceService
        .getActivitiesWithoutContractingPerformance(contractedActivities);
    return contractingPerformanceFormService.getScopeTitlesMap(activitiesWithoutContractingPerformance);
  }

  private Map<String, String> getScopeTitlesMapWithCurrent(List<ActualTenderActivity> contractedActivities,
                                                           ContractingPerformance contractingPerformance) {
    var activitiesWithoutContractingPerformance = contractingPerformanceService
        .getActivitiesWithoutContractingPerformancesWithCurrent(contractedActivities, contractingPerformance);
    return contractingPerformanceFormService.getScopeTitlesMap(activitiesWithoutContractingPerformance);
  }

  private ModelAndView contractingPerformanceFormModelAndView(Map<String, String> scopeTitlesMap,
                                                              String backLinkUrl) {
    return new ModelAndView("scap/scap/contractingperformance/contractingPerformanceDetail")
        .addObject("backLinkUrl", backLinkUrl)
        .addObject("scopeTitlesMap", scopeTitlesMap);
  }

}
