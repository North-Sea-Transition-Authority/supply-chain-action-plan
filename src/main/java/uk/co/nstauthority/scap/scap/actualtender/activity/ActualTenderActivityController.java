package uk.co.nstauthority.scap.scap.actualtender.activity;

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
import uk.co.nstauthority.scap.endpointvalidation.annotations.ScapHasStatus;
import uk.co.nstauthority.scap.energyportal.rest.OrganisationUnitRestController;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.permissionmanagement.endpointsecurity.PermissionsRequiredForScap;
import uk.co.nstauthority.scap.scap.RemunerationModel;
import uk.co.nstauthority.scap.scap.actualtender.ActualTenderControllerRedirectionService;
import uk.co.nstauthority.scap.scap.actualtender.ActualTenderService;
import uk.co.nstauthority.scap.scap.actualtender.hasactualtender.HasActualTenderController;
import uk.co.nstauthority.scap.scap.actualtender.summary.ActualTenderSummaryController;
import uk.co.nstauthority.scap.scap.contractingperformance.ContractingPerformanceService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.scap.ScapService;

@Controller
@RequestMapping("{scapId}/actual-tender/activity")
@PermissionsRequiredForScap(permissions = RolePermission.SUBMIT_SCAP)
@ScapHasStatus(permittedStatuses = ScapDetailStatus.DRAFT)
public class ActualTenderActivityController {

  static final String DELETES_CONTRACTING_PERFORMANCE_WARNING =
      "This actual tendering activity has associated contracting performance. " +
      "On changing the stage of this contract, the related contracting performance information will be deleted.";
  static final String PRESELECTED_ITT_PARTICIPANTS_OBJECT = "preselectedIttParticipants";

  private final ScapService scapService;
  private final ScapDetailService scapDetailService;
  private final ActualTenderService actualTenderService;
  private final ControllerHelperService controllerHelperService;
  private final ActualTenderActivityFormService actualTenderActivityFormService;
  private final ActualTenderActivityService actualTenderActivityService;
  private final ActualTenderControllerRedirectionService actualTenderControllerRedirectionService;
  private final InvitationToTenderParticipantService invitationToTenderParticipantService;
  private final UpdateActualTenderActivityService updateActualTenderActivityService;
  private final ContractingPerformanceService contractingPerformanceService;

  @Autowired
  public ActualTenderActivityController(ScapService scapService, ScapDetailService scapDetailService,
                                        ActualTenderService actualTenderService,
                                        ControllerHelperService controllerHelperService,
                                        ActualTenderActivityFormService actualTenderActivityFormService,
                                        ActualTenderActivityService actualTenderActivityService,
                                        ActualTenderControllerRedirectionService actualTenderControllerRedirectionService,
                                        InvitationToTenderParticipantService invitationToTenderParticipantService,
                                        UpdateActualTenderActivityService updateActualTenderActivityService,
                                        ContractingPerformanceService contractingPerformanceService) {
    this.scapService = scapService;
    this.scapDetailService = scapDetailService;
    this.actualTenderService = actualTenderService;
    this.controllerHelperService = controllerHelperService;
    this.actualTenderActivityFormService = actualTenderActivityFormService;
    this.actualTenderActivityService = actualTenderActivityService;
    this.actualTenderControllerRedirectionService = actualTenderControllerRedirectionService;
    this.invitationToTenderParticipantService = invitationToTenderParticipantService;
    this.updateActualTenderActivityService = updateActualTenderActivityService;
    this.contractingPerformanceService = contractingPerformanceService;
  }

  @GetMapping
  public ModelAndView renderActualTenderActivityForm(@PathVariable("scapId") ScapId scapId,
                                                     @ModelAttribute("form") ActualTenderActivityForm form) {
    var scap = scapService.getScapById(scapId);
    var scapDetail = scapDetailService.getLatestScapDetailByScapOrThrow(scap);
    actualTenderService.getByScapDetailOrThrow(scapDetail);
    var backLinkUrl = ReverseRouter.route(on(HasActualTenderController.class).renderHasActualTenderForm(scapId));

    return actualTenderDetailFormModelAndView(backLinkUrl);
  }

  @PostMapping
  public ModelAndView saveActualTenderActivityForm(@PathVariable("scapId") ScapId scapId,
                                                   @ModelAttribute("form") ActualTenderActivityForm form,
                                                   BindingResult bindingResult) {
    var scap = scapService.getScapById(scapId);
    var scapDetail = scapDetailService.getLatestScapDetailByScapOrThrow(scap);
    var actualTender = actualTenderService.getByScapDetailOrThrow(scapDetail);
    var backLinkUrl = ReverseRouter.route(on(HasActualTenderController.class).renderHasActualTenderForm(scapId));

    bindingResult = actualTenderActivityFormService.validate(form, bindingResult, actualTender);

    var preselectedIttParticipants = actualTenderActivityFormService
        .getPreselectedIttParticipants(form.getInvitationToTenderParticipants(), bindingResult);

    return controllerHelperService.checkErrorsAndRedirect(
        bindingResult,
        actualTenderDetailFormModelAndView(backLinkUrl)
            .addObject(PRESELECTED_ITT_PARTICIPANTS_OBJECT, preselectedIttParticipants),
        form,
        () -> {
          var actualTenderActivity = actualTenderActivityService
              .createActualTenderActivity(actualTender, form);
          invitationToTenderParticipantService.updateInvitationToTenderParticipants(
              actualTenderActivity, form.getInvitationToTenderParticipants());
          return actualTenderControllerRedirectionService
              .redirectFromActualTenderActivityForm(scapId, actualTenderActivity);
        }
    );
  }

  @GetMapping("/{activityId}")
  public ModelAndView renderExistingActualTenderActivityForm(@PathVariable("scapId") ScapId scapId,
                                                             @PathVariable("activityId") Integer activityId) {
    scapService.getScapById(scapId);
    var actualTenderActivity = actualTenderActivityService.getById(activityId);
    var invitationToTenderParticipants = invitationToTenderParticipantService
        .getInvitationToTenderParticipants(actualTenderActivity);
    var form = actualTenderActivityFormService
        .getForm(actualTenderActivity, invitationToTenderParticipants);
    var preselectedIttParticipants = actualTenderActivityFormService
        .getPreselectedIttParticipants(invitationToTenderParticipants);
    var backLinkUrl = ReverseRouter.route(on(ActualTenderSummaryController.class).renderActualTenderSummary(scapId));

    var contractingPerformanceWarning = getContractingPerformanceWarning(actualTenderActivity);

    return actualTenderDetailFormModelAndView(backLinkUrl)
        .addObject("form", form)
        .addObject("contractingPerformanceWarning", contractingPerformanceWarning)
        .addObject(PRESELECTED_ITT_PARTICIPANTS_OBJECT, preselectedIttParticipants);
  }

  @PostMapping("/{activityId}")
  public ModelAndView saveExistingActualTenderActivityForm(@PathVariable("scapId") ScapId scapId,
                                                           @PathVariable("activityId") Integer activityId,
                                                           @ModelAttribute("form") ActualTenderActivityForm form,
                                                           BindingResult bindingResult) {
    var scap = scapService.getScapById(scapId);
    var scapDetail = scapDetailService.getLatestScapDetailByScapOrThrow(scap);
    var actualTender = actualTenderService.getByScapDetailOrThrow(scapDetail);
    var actualTenderActivity = actualTenderActivityService.getById(activityId);
    var backLinkUrl = ReverseRouter.route(on(ActualTenderSummaryController.class).renderActualTenderSummary(scapId));

    bindingResult = actualTenderActivityFormService.validate(form, bindingResult, actualTender, actualTenderActivity);

    var contractingPerformanceWarning = getContractingPerformanceWarning(actualTenderActivity);
    var preselectedIttParticipants = actualTenderActivityFormService
        .getPreselectedIttParticipants(form.getInvitationToTenderParticipants(), bindingResult);

    return controllerHelperService.checkErrorsAndRedirect(
        bindingResult,
        actualTenderDetailFormModelAndView(backLinkUrl)
            .addObject("contractingPerformanceWarning", contractingPerformanceWarning)
            .addObject(PRESELECTED_ITT_PARTICIPANTS_OBJECT, preselectedIttParticipants),
        form,
        () -> {
          updateActualTenderActivityService.updateActualTenderActivity(actualTenderActivity, form);
          invitationToTenderParticipantService.updateInvitationToTenderParticipants(
              actualTenderActivity, form.getInvitationToTenderParticipants());
          return actualTenderControllerRedirectionService.redirectFromActualTenderActivityForm(scapId, actualTenderActivity);
        }
    );
  }

  private ModelAndView actualTenderDetailFormModelAndView(String backLinkUrl) {
    return new ModelAndView("scap/scap/actualtender/actualTenderActivityDetails")
        .addObject("backLinkUrl", backLinkUrl)
        .addObject("remunerationModels", RemunerationModel.getRemunerationModels())
        .addObject("contractStages", ContractStage.getContractStages())
        .addObject("scopeTitleMaxLength", ActualTenderActivityFormValidator.MAX_SCOPE_TITLE_LENGTH.toString())
        .addObject("organisationUnitSearchUrl",
            ReverseRouter.route(on(OrganisationUnitRestController.class).searchOrganisationUnitsWithManualEntry(null)));
  }

  private String getContractingPerformanceWarning(ActualTenderActivity actualTenderActivity) {
    if (contractingPerformanceService.hasContractingPerformance(actualTenderActivity)) {
      return DELETES_CONTRACTING_PERFORMANCE_WARNING;
    }
    return null;
  }
}
