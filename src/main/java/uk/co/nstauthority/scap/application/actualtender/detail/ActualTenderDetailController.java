package uk.co.nstauthority.scap.application.actualtender.detail;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import org.springframework.stereotype.Controller;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;
import uk.co.nstauthority.scap.application.RemunerationModel;
import uk.co.nstauthority.scap.application.actualtender.ActualTenderService;
import uk.co.nstauthority.scap.application.actualtender.hasactualtender.HasActualTenderController;
import uk.co.nstauthority.scap.application.detail.ScapDetailService;
import uk.co.nstauthority.scap.application.overview.ScapOverviewService;
import uk.co.nstauthority.scap.application.tasklist.TaskListController;
import uk.co.nstauthority.scap.controllerhelper.ControllerHelperService;
import uk.co.nstauthority.scap.mvc.ReverseRouter;

@Controller
@RequestMapping("{scapId}/actual-tender/activity")
public class ActualTenderDetailController {

  private final ScapOverviewService scapOverviewService;
  private final ScapDetailService scapDetailService;
  private final ActualTenderService actualTenderService;
  private final ControllerHelperService controllerHelperService;
  private final ActualTenderDetailFormService actualTenderDetailFormService;

  public ActualTenderDetailController(ScapOverviewService scapOverviewService, ScapDetailService scapDetailService,
                                      ActualTenderService actualTenderService,
                                      ControllerHelperService controllerHelperService,
                                      ActualTenderDetailFormService actualTenderDetailFormService) {
    this.scapOverviewService = scapOverviewService;
    this.scapDetailService = scapDetailService;
    this.actualTenderService = actualTenderService;
    this.controllerHelperService = controllerHelperService;
    this.actualTenderDetailFormService = actualTenderDetailFormService;
  }

  @GetMapping
  public ModelAndView renderActualTenderDetailForm(@PathVariable("scapId") Integer scapId,
                                                   @ModelAttribute("form") ActualTenderDetailForm form) {
    var scap = scapOverviewService.getScapById(scapId);
    var scapDetail = scapDetailService.getLatestScapDetailByScapOrThrow(scap);
    actualTenderService.getByScapDetailOrThrow(scapDetail);

    return actualTenderDetailFormModelAndView(scapId);
  }

  @PostMapping
  public ModelAndView saveActualTenderDetailForm(@PathVariable("scapId") Integer scapId,
                                                 @ModelAttribute("form") ActualTenderDetailForm form,
                                                 BindingResult bindingResult) {
    var scap = scapOverviewService.getScapById(scapId);
    var scapDetail = scapDetailService.getLatestScapDetailByScapOrThrow(scap);
    actualTenderService.getByScapDetailOrThrow(scapDetail);

    bindingResult = actualTenderDetailFormService.validate(form, bindingResult);

    return controllerHelperService.checkErrorsAndRedirect(
        bindingResult,
        actualTenderDetailFormModelAndView(scapId),
        form,
        // TODO SCAP2022-43: Update this to redirect to actual tendering activity summary
        () -> ReverseRouter.redirect(on(TaskListController.class).renderTaskList(scapId))
    );
  }

  private ModelAndView actualTenderDetailFormModelAndView(Integer scapId) {
    return new ModelAndView("scap/application/actualtender/actualTenderActivityDetail")
        .addObject("backLinkUrl", ReverseRouter.route(on(HasActualTenderController.class).renderHasActualTenderForm(scapId)))
        .addObject("remunerationModels", RemunerationModel.getRemunerationModels())
        .addObject("contractStages", ContractStage.getContractStages());
  }
}
