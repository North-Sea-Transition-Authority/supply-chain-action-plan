package uk.co.nstauthority.scap.application.plannedtender.hasplannedtender;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;
import static uk.co.nstauthority.scap.mvc.ReverseRouter.emptyBindingResult;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;
import uk.co.nstauthority.scap.application.detail.ScapDetailService;
import uk.co.nstauthority.scap.application.overview.ScapOverviewService;
import uk.co.nstauthority.scap.application.plannedtender.ScapPlannedTenderController;
import uk.co.nstauthority.scap.application.plannedtender.ScapPlannedTenderService;
import uk.co.nstauthority.scap.application.plannedtender.detail.ScapPlannedTenderDetailController;
import uk.co.nstauthority.scap.application.plannedtender.detail.ScapPlannedTenderDetailService;
import uk.co.nstauthority.scap.application.tasklist.TaskListController;
import uk.co.nstauthority.scap.enumutil.YesNo;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.validation.ValidationErrorOrderingService;

@Controller
@RequestMapping("{scapId}/planned-tender")
public class ScapHasPlannedTenderController {

  private final ScapOverviewService scapOverviewService;
  private final ScapDetailService scapDetailService;
  private final ScapPlannedTenderService scapPlannedTenderService;
  private final ScapHasPlannedTenderFormService scapHasPlannedTenderFormService;
  private final ScapPlannedTenderDetailService scapPlannedTenderDetailService;
  private final ValidationErrorOrderingService validationErrorOrderingService;

  @Autowired
  public ScapHasPlannedTenderController(ScapOverviewService scapOverviewService,
                                        ScapDetailService scapDetailService,
                                        ScapPlannedTenderService scapPlannedTenderService,
                                        ScapHasPlannedTenderFormService scapHasPlannedTenderFormService,
                                        ScapPlannedTenderDetailService scapPlannedTenderDetailService,
                                        ValidationErrorOrderingService validationErrorOrderingService) {
    this.scapOverviewService = scapOverviewService;
    this.scapDetailService = scapDetailService;
    this.scapPlannedTenderService = scapPlannedTenderService;
    this.scapHasPlannedTenderFormService = scapHasPlannedTenderFormService;
    this.scapPlannedTenderDetailService = scapPlannedTenderDetailService;
    this.validationErrorOrderingService = validationErrorOrderingService;
  }

  @GetMapping
  public ModelAndView renderHasPlannedTenderActivityForm(@PathVariable("scapId") Integer scapId) {
    var scap = scapOverviewService.getScapById(scapId);
    var scapDetail = scapDetailService.getLatestScapDetailByScapOrThrow(scap);
    var form = scapHasPlannedTenderFormService.getForm(scapDetail);
    var existingPlannedTender = scapPlannedTenderService.getScapPlannedTenderByScapDetail(scapDetail);
    if (existingPlannedTender.map(scapPlannedTenderDetailService::hasExistingTenderDetails).orElse(false)) {
      return ReverseRouter.redirect(on(ScapPlannedTenderController.class).renderPlannedTenderActivities(scapId));
    }
    return hasPlannedTenderActivityModelAndView(scapId, form);
  }

  @PostMapping
  public ModelAndView saveHasPlannedTenderActivity(@PathVariable("scapId") Integer scapId,
                                                   @ModelAttribute("form") ScapHasPlannedTenderForm form,
                                                   BindingResult bindingResult) {
    var scap = scapOverviewService.getScapById(scapId);
    var scapDetail = scapDetailService.getLatestScapDetailByScapOrThrow(scap);
    var existingPlannedTender = scapPlannedTenderService.getScapPlannedTenderByScapDetail(scapDetail);

    if (existingPlannedTender.map(scapPlannedTenderDetailService::hasExistingTenderDetails).orElse(false)) {
      return ReverseRouter.redirect(on(ScapPlannedTenderController.class).renderPlannedTenderActivities(scapId));
    }

    bindingResult = scapHasPlannedTenderFormService.validate(form, bindingResult);
    if (bindingResult.hasErrors()) {
      return hasPlannedTenderActivityModelAndView(scapId, form)
          .addObject("errorItems", validationErrorOrderingService.getErrorItemsFromBindingResult(form, bindingResult));
    }

    var plannedTender = existingPlannedTender
        .orElseGet(() -> scapPlannedTenderService.createPlannedTenderForScapDetail(scapDetail));
    if (YesNo.NO.equals(form.getHasPlannedTender())) {
      scapPlannedTenderService.updatePlannedTenderHasPlannedTenders(plannedTender, false);
      return ReverseRouter.redirect(on(TaskListController.class).renderTaskList(scapId));
    }

    scapPlannedTenderService.updatePlannedTenderHasPlannedTenders(plannedTender, true);
    return ReverseRouter.redirect(on(ScapPlannedTenderDetailController.class).renderPlannedTenderDetailForm(scapId, null));
  }

  private ModelAndView hasPlannedTenderActivityModelAndView(Integer scapId, ScapHasPlannedTenderForm form) {
    return new ModelAndView("scap/application/plannedTender/hasPlannedTender")
        .addObject("backLinkUrl", ReverseRouter.route(on(TaskListController.class).renderTaskList(scapId)))
        .addObject("hasPlannedTender", YesNo.getHasPlannedTender())
        .addObject("form", form)
        .addObject("submitPostUrl",
            ReverseRouter.route(on(ScapHasPlannedTenderController.class)
                .saveHasPlannedTenderActivity(scapId, null, emptyBindingResult())));
  }

}
