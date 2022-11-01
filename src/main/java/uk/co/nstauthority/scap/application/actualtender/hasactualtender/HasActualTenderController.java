package uk.co.nstauthority.scap.application.actualtender.hasactualtender;

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
import uk.co.nstauthority.scap.application.overview.ScapOverviewService;
import uk.co.nstauthority.scap.application.tasklist.TaskListController;
import uk.co.nstauthority.scap.controllerhelper.ControllerHelperService;
import uk.co.nstauthority.scap.enumutil.YesNo;
import uk.co.nstauthority.scap.mvc.ReverseRouter;

@Controller
@RequestMapping("{scapId}/actual-tender")
public class HasActualTenderController {

  private final ScapOverviewService scapOverviewService;
  private final ControllerHelperService controllerHelperService;
  private final HasActualTenderFormService hasActualTenderFormService;

  @Autowired
  HasActualTenderController(ScapOverviewService scapOverviewService, ControllerHelperService controllerHelperService,
                            HasActualTenderFormService hasActualTenderFormService) {
    this.scapOverviewService = scapOverviewService;
    this.controllerHelperService = controllerHelperService;
    this.hasActualTenderFormService = hasActualTenderFormService;
  }

  @GetMapping
  public ModelAndView renderHasActualTenderForm(@PathVariable("scapId") Integer scapId) {
    scapOverviewService.getScapById(scapId);
    var form = hasActualTenderFormService.getForm();

    return hasActualTenderFormModelAndView(scapId, form);
  }

  @PostMapping
  ModelAndView saveHasActualTenderForm(@PathVariable("scapId") Integer scapId,
                                       @ModelAttribute("form") HasActualTenderForm form,
                                       BindingResult bindingResult) {
    scapOverviewService.getScapById(scapId);
    bindingResult = hasActualTenderFormService.validate(form, bindingResult);

    return controllerHelperService.checkErrorsAndRedirect(
        bindingResult,
        hasActualTenderFormModelAndView(scapId, form),
        form,
        () -> ReverseRouter.redirect(on(TaskListController.class).renderTaskList(scapId))
    );
  }

  private ModelAndView hasActualTenderFormModelAndView(Integer scapId, HasActualTenderForm form) {
    return new ModelAndView("scap/application/actualtender/hasActualTender")
        .addObject("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(scapId)))
        .addObject("hasActualTender", YesNo.getRadioOptions())
        .addObject("form", form);
  }
}
