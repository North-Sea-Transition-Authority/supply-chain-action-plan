package uk.co.nstauthority.scap.scap.actualtender.activity;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

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
import uk.co.nstauthority.scap.scap.actualtender.ActualTenderService;
import uk.co.nstauthority.scap.scap.actualtender.hasactualtender.HasActualTenderController;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.scap.ScapService;
import uk.co.nstauthority.scap.scap.tasklist.TaskListController;

@Controller
@RequestMapping("{scapId}/actual-tender/activity")
public class ActualTenderActivityController {

  private final ScapService scapService;
  private final ScapDetailService scapDetailService;
  private final ActualTenderService actualTenderService;
  private final ControllerHelperService controllerHelperService;
  private final ActualTenderActivityFormService actualTenderActivityFormService;
  private final ActualTenderActivityService actualTenderActivityService;

  public ActualTenderActivityController(ScapService scapService, ScapDetailService scapDetailService,
                                        ActualTenderService actualTenderService,
                                        ControllerHelperService controllerHelperService,
                                        ActualTenderActivityFormService actualTenderActivityFormService,
                                        ActualTenderActivityService actualTenderActivityService) {
    this.scapService = scapService;
    this.scapDetailService = scapDetailService;
    this.actualTenderService = actualTenderService;
    this.controllerHelperService = controllerHelperService;
    this.actualTenderActivityFormService = actualTenderActivityFormService;
    this.actualTenderActivityService = actualTenderActivityService;
  }

  @GetMapping
  public ModelAndView renderActualTenderDetailForm(@PathVariable("scapId") Integer scapId,
                                                   @ModelAttribute("form") ActualTenderActivityForm form) {
    var scap = scapService.getScapById(scapId);
    var scapDetail = scapDetailService.getLatestScapDetailByScapOrThrow(scap);
    actualTenderService.getByScapDetailOrThrow(scapDetail);

    return actualTenderDetailFormModelAndView(scapId);
  }

  @PostMapping
  public ModelAndView saveActualTenderDetailForm(@PathVariable("scapId") Integer scapId,
                                                 @ModelAttribute("form") ActualTenderActivityForm form,
                                                 BindingResult bindingResult) {
    var scap = scapService.getScapById(scapId);
    var scapDetail = scapDetailService.getLatestScapDetailByScapOrThrow(scap);
    var actualTender = actualTenderService.getByScapDetailOrThrow(scapDetail);

    bindingResult = actualTenderActivityFormService.validate(form, bindingResult);

    return controllerHelperService.checkErrorsAndRedirect(
        bindingResult,
        actualTenderDetailFormModelAndView(scapId),
        form,
        // TODO SCAP2022-43: Update this to redirect to actual tendering activity summary
        () -> {
          actualTenderActivityService.createActualTenderDetail(actualTender, form);
          return ReverseRouter.redirect(on(TaskListController.class).renderTaskList(scapId));
        }
    );
  }

  private ModelAndView actualTenderDetailFormModelAndView(Integer scapId) {
    return new ModelAndView("scap/application/actualtender/actualTenderActivityDetail")
        .addObject("backLinkUrl", ReverseRouter.route(on(HasActualTenderController.class).renderHasActualTenderForm(scapId)))
        .addObject("remunerationModels", RemunerationModel.getRemunerationModels())
        .addObject("contractStages", ContractStage.getContractStages());
  }
}
