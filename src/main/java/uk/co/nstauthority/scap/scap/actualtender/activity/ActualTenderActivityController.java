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
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.RemunerationModel;
import uk.co.nstauthority.scap.scap.actualtender.ActualTenderControllerRedirectionService;
import uk.co.nstauthority.scap.scap.actualtender.ActualTenderService;
import uk.co.nstauthority.scap.scap.actualtender.hasactualtender.HasActualTenderController;
import uk.co.nstauthority.scap.scap.actualtender.summary.ActualTenderSummaryController;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.scap.ScapService;

@Controller
@RequestMapping("{scapId}/actual-tender/activity")
public class ActualTenderActivityController {

  private final ScapService scapService;
  private final ScapDetailService scapDetailService;
  private final ActualTenderService actualTenderService;
  private final ControllerHelperService controllerHelperService;
  private final ActualTenderActivityFormService actualTenderActivityFormService;
  private final ActualTenderActivityService actualTenderActivityService;
  private final ActualTenderControllerRedirectionService actualTenderControllerRedirectionService;
  private final InvitationToTenderParticipantService invitationToTenderParticipantService;

  @Autowired
  public ActualTenderActivityController(ScapService scapService, ScapDetailService scapDetailService,
                                        ActualTenderService actualTenderService,
                                        ControllerHelperService controllerHelperService,
                                        ActualTenderActivityFormService actualTenderActivityFormService,
                                        ActualTenderActivityService actualTenderActivityService,
                                        ActualTenderControllerRedirectionService actualTenderControllerRedirectionService,
                                        InvitationToTenderParticipantService invitationToTenderParticipantService) {
    this.scapService = scapService;
    this.scapDetailService = scapDetailService;
    this.actualTenderService = actualTenderService;
    this.controllerHelperService = controllerHelperService;
    this.actualTenderActivityFormService = actualTenderActivityFormService;
    this.actualTenderActivityService = actualTenderActivityService;
    this.actualTenderControllerRedirectionService = actualTenderControllerRedirectionService;
    this.invitationToTenderParticipantService = invitationToTenderParticipantService;
  }

  @GetMapping
  public ModelAndView renderActualTenderActivityForm(@PathVariable("scapId") Integer scapId,
                                                     @ModelAttribute("form") ActualTenderActivityForm form) {
    var scap = scapService.getScapById(scapId);
    var scapDetail = scapDetailService.getLatestScapDetailByScapOrThrow(scap);
    actualTenderService.getByScapDetailOrThrow(scapDetail);
    var backLinkUrl = ReverseRouter.route(on(HasActualTenderController.class).renderHasActualTenderForm(scapId));

    return actualTenderDetailFormModelAndView(backLinkUrl);
  }

  @PostMapping
  public ModelAndView saveActualTenderActivityForm(@PathVariable("scapId") Integer scapId,
                                                   @ModelAttribute("form") ActualTenderActivityForm form,
                                                   BindingResult bindingResult) {
    var scap = scapService.getScapById(scapId);
    var scapDetail = scapDetailService.getLatestScapDetailByScapOrThrow(scap);
    var actualTender = actualTenderService.getByScapDetailOrThrow(scapDetail);
    var backLinkUrl = ReverseRouter.route(on(HasActualTenderController.class).renderHasActualTenderForm(scapId));

    bindingResult = actualTenderActivityFormService.validate(form, bindingResult);

    return controllerHelperService.checkErrorsAndRedirect(
        bindingResult,
        actualTenderDetailFormModelAndView(backLinkUrl),
        form,
        () -> {
          var actualTenderActivity = actualTenderActivityService
              .createActualTenderActivity(actualTender, form);
          return actualTenderControllerRedirectionService
              .redirectFromActualTenderActivityForm(scapId, actualTenderActivity);
        }
    );
  }

  @GetMapping("/{activityId}")
  public ModelAndView renderExistingActualTenderActivityForm(@PathVariable("scapId") Integer scapId,
                                                             @PathVariable("activityId") Integer activityId) {
    scapService.getScapById(scapId);
    var actualTenderActivity = actualTenderActivityService.getById(activityId);
    var invitationToTenderParticipants = invitationToTenderParticipantService
        .getInvitationToTenderParticipants(actualTenderActivity);
    var form = actualTenderActivityFormService
        .getForm(actualTenderActivity, invitationToTenderParticipants);
    var backLinkUrl = ReverseRouter.route(on(ActualTenderSummaryController.class).renderActualTenderSummary(scapId));

    return actualTenderDetailFormModelAndView(backLinkUrl)
        .addObject("form", form);
  }

  @PostMapping("/{activityId}")
  public ModelAndView saveExistingActualTenderActivityForm(@PathVariable("scapId") Integer scapId,
                                                           @PathVariable("activityId") Integer activityId,
                                                           @ModelAttribute("form") ActualTenderActivityForm form,
                                                           BindingResult bindingResult) {
    scapService.getScapById(scapId);
    var actualTenderActivity = actualTenderActivityService.getById(activityId);
    var backLinkUrl = ReverseRouter.route(on(ActualTenderSummaryController.class).renderActualTenderSummary(scapId));

    bindingResult = actualTenderActivityFormService.validate(form, bindingResult);

    return controllerHelperService.checkErrorsAndRedirect(
        bindingResult,
        actualTenderDetailFormModelAndView(backLinkUrl),
        form,
        () -> {
          actualTenderActivityService.updateActualTenderActivity(actualTenderActivity, form);
          return actualTenderControllerRedirectionService.redirectFromActualTenderActivityForm(scapId, actualTenderActivity);
        }
    );
  }

  private ModelAndView actualTenderDetailFormModelAndView(String backLinkUrl) {
    return new ModelAndView("scap/scap/actualtender/actualTenderActivityDetails")
        .addObject("backLinkUrl", backLinkUrl)
        .addObject("remunerationModels", RemunerationModel.getRemunerationModels())
        .addObject("contractStages", ContractStage.getContractStages());
  }
}
