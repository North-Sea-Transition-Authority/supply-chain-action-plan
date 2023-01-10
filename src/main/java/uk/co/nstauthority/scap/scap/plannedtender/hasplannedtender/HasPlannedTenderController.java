package uk.co.nstauthority.scap.scap.plannedtender.hasplannedtender;

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
import uk.co.nstauthority.scap.enumutil.YesNo;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.plannedtender.PlannedTenderController;
import uk.co.nstauthority.scap.scap.plannedtender.PlannedTenderService;
import uk.co.nstauthority.scap.scap.plannedtender.activity.PlannedTenderActivityController;
import uk.co.nstauthority.scap.scap.plannedtender.activity.PlannedTenderActivityService;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.scap.ScapService;
import uk.co.nstauthority.scap.scap.tasklist.TaskListController;
import uk.co.nstauthority.scap.validation.ValidationErrorOrderingService;

@Controller
@RequestMapping("{scapId}/planned-tender")
public class HasPlannedTenderController {

  private final ScapService scapService;
  private final ScapDetailService scapDetailService;
  private final PlannedTenderService plannedTenderService;
  private final HasPlannedTenderFormService hasPlannedTenderFormService;
  private final PlannedTenderActivityService plannedTenderActivityService;
  private final ValidationErrorOrderingService validationErrorOrderingService;

  @Autowired
  public HasPlannedTenderController(ScapService scapService,
                                    ScapDetailService scapDetailService,
                                    PlannedTenderService plannedTenderService,
                                    HasPlannedTenderFormService hasPlannedTenderFormService,
                                    PlannedTenderActivityService plannedTenderActivityService,
                                    ValidationErrorOrderingService validationErrorOrderingService) {
    this.scapService = scapService;
    this.scapDetailService = scapDetailService;
    this.plannedTenderService = plannedTenderService;
    this.hasPlannedTenderFormService = hasPlannedTenderFormService;
    this.plannedTenderActivityService = plannedTenderActivityService;
    this.validationErrorOrderingService = validationErrorOrderingService;
  }

  @GetMapping
  public ModelAndView renderHasPlannedTenderActivityForm(@PathVariable("scapId") ScapId scapId) {
    var scap = scapService.getScapById(scapId);
    var scapDetail = scapDetailService.getLatestScapDetailByScapOrThrow(scap);
    var form = hasPlannedTenderFormService.getForm(scapDetail);
    var existingPlannedTender = plannedTenderService.getScapPlannedTenderByScapDetail(scapDetail);
    if (existingPlannedTender.map(plannedTenderActivityService::hasExistingTenderDetails).orElse(false)) {
      return ReverseRouter.redirect(on(PlannedTenderController.class).renderPlannedTenderActivities(scapId));
    }
    return hasPlannedTenderActivityModelAndView(scapId, form);
  }

  @PostMapping
  public ModelAndView saveHasPlannedTenderActivity(@PathVariable("scapId") ScapId scapId,
                                                   @ModelAttribute("form") HasPlannedTenderForm form,
                                                   BindingResult bindingResult) {
    var scap = scapService.getScapById(scapId);
    var scapDetail = scapDetailService.getLatestScapDetailByScapOrThrow(scap);
    var existingPlannedTender = plannedTenderService.getScapPlannedTenderByScapDetail(scapDetail);

    if (existingPlannedTender.map(plannedTenderActivityService::hasExistingTenderDetails).orElse(false)) {
      return ReverseRouter.redirect(on(PlannedTenderController.class).renderPlannedTenderActivities(scapId));
    }

    bindingResult = hasPlannedTenderFormService.validate(form, bindingResult);
    if (bindingResult.hasErrors()) {
      return hasPlannedTenderActivityModelAndView(scapId, form)
          .addObject("errorItems", validationErrorOrderingService.getErrorItemsFromBindingResult(form, bindingResult));
    }

    var plannedTender = existingPlannedTender
        .orElseGet(() -> plannedTenderService.createPlannedTenderForScapDetail(scapDetail));
    if (YesNo.NO.equals(form.getHasPlannedTender())) {
      plannedTenderService.updatePlannedTenderHasPlannedTenders(plannedTender, false);
      return ReverseRouter.redirect(on(TaskListController.class).renderTaskList(scapId));
    }

    plannedTenderService.updatePlannedTenderHasPlannedTenders(plannedTender, true);
    return ReverseRouter.redirect(on(PlannedTenderActivityController.class).renderPlannedTenderDetailForm(scapId, null));
  }

  private ModelAndView hasPlannedTenderActivityModelAndView(ScapId scapId, HasPlannedTenderForm form) {
    return new ModelAndView("scap/scap/plannedtender/hasPlannedTender")
        .addObject("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(scapId)))
        .addObject("hasPlannedTender", YesNo.getRadioOptions())
        .addObject("form", form)
        .addObject("submitPostUrl",
            ReverseRouter.route(on(HasPlannedTenderController.class)
                .saveHasPlannedTenderActivity(scapId, null, emptyBindingResult())));
  }

}
