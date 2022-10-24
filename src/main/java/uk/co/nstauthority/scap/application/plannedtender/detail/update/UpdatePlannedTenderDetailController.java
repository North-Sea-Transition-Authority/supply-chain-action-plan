package uk.co.nstauthority.scap.application.plannedtender.detail.update;

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
import uk.co.nstauthority.scap.application.plannedtender.ScapPlannedTenderController;
import uk.co.nstauthority.scap.application.plannedtender.detail.RemunerationModel;
import uk.co.nstauthority.scap.application.plannedtender.detail.ScapPlannedTenderDetailForm;
import uk.co.nstauthority.scap.application.plannedtender.detail.ScapPlannedTenderDetailFormService;
import uk.co.nstauthority.scap.application.plannedtender.detail.ScapPlannedTenderDetailService;
import uk.co.nstauthority.scap.mvc.ReverseRouter;

@Controller
@RequestMapping("{scapId}/planned-tender/{plannedTenderDetailId}/update")
public class UpdatePlannedTenderDetailController {

  private final ScapOverviewService scapOverviewService;
  private final ScapPlannedTenderDetailService scapPlannedTenderDetailService;
  private final ScapPlannedTenderDetailFormService scapPlannedTenderDetailFormService;

  @Autowired
  UpdatePlannedTenderDetailController(ScapOverviewService scapOverviewService,
                                      ScapPlannedTenderDetailService scapPlannedTenderDetailService,
                                      ScapPlannedTenderDetailFormService scapPlannedTenderDetailFormService) {
    this.scapOverviewService = scapOverviewService;
    this.scapPlannedTenderDetailService = scapPlannedTenderDetailService;
    this.scapPlannedTenderDetailFormService = scapPlannedTenderDetailFormService;
  }

  @GetMapping
  public ModelAndView renderUpdatePlannedTenderDetail(@PathVariable("scapId") Integer scapId,
                                                      @PathVariable("plannedTenderDetailId") Integer plannedTenderDetailId) {
    scapOverviewService.getScapById(scapId);
    var plannedTenderDetail = scapPlannedTenderDetailService.getPlannedTenderDetailById(plannedTenderDetailId);
    var form = scapPlannedTenderDetailFormService.getForm(plannedTenderDetail);

    return plannedTenderDetailFormModelAndView(scapId, form);
  }

  @PostMapping
  public ModelAndView saveUpdatedPlannedTenderDetail(@PathVariable("scapId") Integer scapId,
                                                    @PathVariable("plannedTenderDetailId") Integer plannedTenderDetailId,
                                                    @ModelAttribute("form") ScapPlannedTenderDetailForm form,
                                                    BindingResult bindingResult) {
    scapOverviewService.getScapById(scapId);
    var plannedTenderDetail = scapPlannedTenderDetailService.getPlannedTenderDetailById(plannedTenderDetailId);

    bindingResult = scapPlannedTenderDetailFormService.validate(bindingResult, form);

    if (bindingResult.hasErrors()) {
      return plannedTenderDetailFormModelAndView(scapId, form);
    }

    scapPlannedTenderDetailService.updatePlannedTenderDetail(plannedTenderDetail, form);

    return ReverseRouter.redirect(on(ScapPlannedTenderController.class).renderPlannedTenderActivities(scapId));

  }


  private ModelAndView plannedTenderDetailFormModelAndView(Integer scapId, ScapPlannedTenderDetailForm form) {
    return new ModelAndView("scap/application/plannedtender/plannedTenderActivityDetail")
        .addObject("backLinkUrl",
            ReverseRouter.route(on(ScapPlannedTenderController.class).renderPlannedTenderActivities(scapId)))
        .addObject("remunerationModels", RemunerationModel.getRemunerationModels())
        .addObject("form", form);
  }
}
