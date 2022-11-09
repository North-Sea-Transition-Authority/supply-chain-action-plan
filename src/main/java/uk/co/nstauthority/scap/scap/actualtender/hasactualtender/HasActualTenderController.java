package uk.co.nstauthority.scap.scap.actualtender.hasactualtender;

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
import uk.co.nstauthority.scap.scap.actualtender.ActualTenderService;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivityController;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.scap.ScapService;
import uk.co.nstauthority.scap.scap.tasklist.TaskListController;

@Controller
@RequestMapping("{scapId}/actual-tender")
public class HasActualTenderController {

  private final ScapService scapService;
  private final ScapDetailService scapDetailService;
  private final ActualTenderService actualTenderService;
  private final ControllerHelperService controllerHelperService;
  private final HasActualTenderFormService hasActualTenderFormService;

  @Autowired
  HasActualTenderController(ScapService scapService, ScapDetailService scapDetailService,
                            ActualTenderService actualTenderService, ControllerHelperService controllerHelperService,
                            HasActualTenderFormService hasActualTenderFormService) {
    this.scapService = scapService;
    this.scapDetailService = scapDetailService;
    this.actualTenderService = actualTenderService;
    this.controllerHelperService = controllerHelperService;
    this.hasActualTenderFormService = hasActualTenderFormService;
  }

  @GetMapping
  public ModelAndView renderHasActualTenderForm(@PathVariable("scapId") Integer scapId) {
    var scap = scapService.getScapById(scapId);
    var scapDetail = scapDetailService.getLatestScapDetailByScapOrThrow(scap);
    var actualTender = actualTenderService.getByScapDetail(scapDetail);
    var form = actualTender
        .map(hasActualTenderFormService::getForm)
        .orElse(new HasActualTenderForm());

    return hasActualTenderFormModelAndView(scapId, form);
  }

  @PostMapping
  ModelAndView saveHasActualTenderForm(@PathVariable("scapId") Integer scapId,
                                       @ModelAttribute("form") HasActualTenderForm form,
                                       BindingResult bindingResult) {
    var scap = scapService.getScapById(scapId);
    var scapDetail = scapDetailService.getLatestScapDetailByScapOrThrow(scap);
    var actualTender = actualTenderService.getByScapDetail(scapDetail);
    bindingResult = hasActualTenderFormService.validate(form, bindingResult);

    return controllerHelperService.checkErrorsAndRedirect(
        bindingResult,
        hasActualTenderFormModelAndView(scapId, form),
        form,
        () -> {
          actualTender.ifPresentOrElse(
              existingActualTender -> actualTenderService.updateHasActualTenders(existingActualTender, form.getHasActualTender()),
              () -> actualTenderService.createActualTender(scapDetail, form.getHasActualTender())
          );
          if (YesNo.YES.equals(form.getHasActualTender())) {
            return ReverseRouter.redirect(on(ActualTenderActivityController.class)
                .renderActualTenderDetailForm(scapId, null));
          }
          return ReverseRouter.redirect(on(TaskListController.class).renderTaskList(scapId));
        });
  }

  private ModelAndView hasActualTenderFormModelAndView(Integer scapId, HasActualTenderForm form) {
    return new ModelAndView("scap/application/actualtender/hasActualTender")
        .addObject("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(scapId)))
        .addObject("hasActualTender", YesNo.getRadioOptions())
        .addObject("form", form);
  }
}
