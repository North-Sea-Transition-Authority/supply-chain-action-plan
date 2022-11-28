package uk.co.nstauthority.scap.scap.plannedtender.activity.update;

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
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.RemunerationModel;
import uk.co.nstauthority.scap.scap.plannedtender.PlannedTenderController;
import uk.co.nstauthority.scap.scap.plannedtender.activity.PlannedTenderActivityForm;
import uk.co.nstauthority.scap.scap.plannedtender.activity.PlannedTenderActivityFormService;
import uk.co.nstauthority.scap.scap.plannedtender.activity.PlannedTenderActivityService;
import uk.co.nstauthority.scap.scap.scap.ScapService;

@Controller
@RequestMapping("{scapId}/planned-tender/{plannedTenderDetailId}/update")
public class UpdatePlannedTenderActivityController {

  private final ScapService scapService;
  private final PlannedTenderActivityService plannedTenderActivityService;
  private final PlannedTenderActivityFormService plannedTenderActivityFormService;

  @Autowired
  UpdatePlannedTenderActivityController(ScapService scapService,
                                        PlannedTenderActivityService plannedTenderActivityService,
                                        PlannedTenderActivityFormService plannedTenderActivityFormService) {
    this.scapService = scapService;
    this.plannedTenderActivityService = plannedTenderActivityService;
    this.plannedTenderActivityFormService = plannedTenderActivityFormService;
  }

  @GetMapping
  public ModelAndView renderUpdatePlannedTenderDetail(@PathVariable("scapId") Integer scapId,
                                                      @PathVariable("plannedTenderDetailId") Integer plannedTenderDetailId) {
    scapService.getScapById(scapId);
    var plannedTenderDetail = plannedTenderActivityService.getPlannedTenderDetailById(plannedTenderDetailId);
    var form = plannedTenderActivityFormService.getForm(plannedTenderDetail);

    return plannedTenderDetailFormModelAndView(scapId, form);
  }

  @PostMapping
  public ModelAndView saveUpdatedPlannedTenderDetail(@PathVariable("scapId") Integer scapId,
                                                    @PathVariable("plannedTenderDetailId") Integer plannedTenderDetailId,
                                                    @ModelAttribute("form") PlannedTenderActivityForm form,
                                                    BindingResult bindingResult) {
    scapService.getScapById(scapId);
    var plannedTenderDetail = plannedTenderActivityService.getPlannedTenderDetailById(plannedTenderDetailId);

    bindingResult = plannedTenderActivityFormService.validate(bindingResult, form);

    if (bindingResult.hasErrors()) {
      return plannedTenderDetailFormModelAndView(scapId, form);
    }

    plannedTenderActivityService.updatePlannedTenderDetail(plannedTenderDetail, form);

    return ReverseRouter.redirect(on(PlannedTenderController.class).renderPlannedTenderActivities(scapId));

  }


  private ModelAndView plannedTenderDetailFormModelAndView(Integer scapId, PlannedTenderActivityForm form) {
    return new ModelAndView("scap/scap/plannedtender/plannedTenderActivityDetails")
        .addObject("backLinkUrl",
            ReverseRouter.route(on(PlannedTenderController.class).renderPlannedTenderActivities(scapId)))
        .addObject("remunerationModels", RemunerationModel.getRemunerationModels())
        .addObject("form", form);
  }
}
