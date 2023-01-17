package uk.co.nstauthority.scap.scap.plannedtender;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.List;
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
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.plannedtender.activity.PlannedTenderActivity;
import uk.co.nstauthority.scap.scap.plannedtender.activity.PlannedTenderActivityController;
import uk.co.nstauthority.scap.scap.plannedtender.activity.PlannedTenderActivityService;
import uk.co.nstauthority.scap.scap.plannedtender.hasplannedtender.HasPlannedTenderController;
import uk.co.nstauthority.scap.scap.plannedtender.list.PlannedTenderActivityListService;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.scap.ScapService;
import uk.co.nstauthority.scap.scap.tasklist.TaskListController;

@Controller
@RequestMapping("{scapId}/planned-tender/activity-summary")
@ScapHasStatus(permittedStatuses = ScapDetailStatus.DRAFT)
public class PlannedTenderController {

  private final ScapService scapService;
  private final ScapDetailService scapDetailService;
  private final PlannedTenderService plannedTenderService;
  private final PlannedTenderActivityService plannedTenderActivityService;
  private final PlannedTenderActivityListService plannedTenderActivityListService;
  private final PlannedTenderFormService plannedTenderFormService;
  private final ControllerHelperService controllerHelperService;

  public PlannedTenderController(ScapService scapService, ScapDetailService scapDetailService,
                                 PlannedTenderService plannedTenderService,
                                 PlannedTenderActivityService plannedTenderActivityService,
                                 PlannedTenderActivityListService plannedTenderActivityListService,
                                 PlannedTenderFormService plannedTenderFormService,
                                 ControllerHelperService controllerHelperService) {
    this.scapService = scapService;
    this.scapDetailService = scapDetailService;
    this.plannedTenderService = plannedTenderService;
    this.plannedTenderActivityService = plannedTenderActivityService;
    this.plannedTenderActivityListService = plannedTenderActivityListService;
    this.plannedTenderFormService = plannedTenderFormService;
    this.controllerHelperService = controllerHelperService;
  }

  @GetMapping
  public ModelAndView renderPlannedTenderActivities(@PathVariable("scapId") ScapId scapId) {
    var scap = scapService.getScapById(scapId);
    var scapDetail = scapDetailService.getLatestScapDetailByScapOrThrow(scap);
    var plannedTender = plannedTenderService.getScapPlannedTenderByScapDetailOrThrow(scapDetail);
    var existingTenderDetails = plannedTenderActivityService.getTenderDetailsByPlannedTender(plannedTender);
    var form = plannedTenderFormService.getForm(plannedTender);

    if (existingTenderDetails.isEmpty()) {
      return ReverseRouter.redirect(on(HasPlannedTenderController.class).renderHasPlannedTenderActivityForm(scapId));
    }

    return plannedTenderActivitySummaryModelAndView(scapId, existingTenderDetails, form);
  }

  @PostMapping
  public ModelAndView saveAnotherPlannedTenderActivity(@PathVariable("scapId") ScapId scapId,
                                                       @ModelAttribute("form") PlannedTenderForm form,
                                                       BindingResult bindingResult) {
    var scap = scapService.getScapById(scapId);
    var scapDetail = scapDetailService.getLatestScapDetailByScapOrThrow(scap);
    var plannedTender = plannedTenderService.getScapPlannedTenderByScapDetailOrThrow(scapDetail);
    var existingTenderDetails = plannedTenderActivityService.getTenderDetailsByPlannedTender(plannedTender);
    bindingResult = plannedTenderFormService.validate(form, bindingResult);

    return controllerHelperService.checkErrorsAndRedirect(
        bindingResult,
        plannedTenderActivitySummaryModelAndView(scapId, existingTenderDetails, form),
        form,
        () -> {
          if (HasMorePlannedTenderActivities.YES_NOW.equals(form.getHasMorePlannedTenderActivities())) {
            return ReverseRouter.redirect(on(PlannedTenderActivityController.class)
                .renderPlannedTenderDetailForm(scapId, null));
          }

          plannedTenderService
              .updatePlannedTenderHasMorePlannedTenders(plannedTender, form.getHasMorePlannedTenderActivities());

          return ReverseRouter.redirect(on(TaskListController.class).renderTaskList(scapId));
        });
  }

  private ModelAndView plannedTenderActivitySummaryModelAndView(ScapId scapId,
                                                                List<PlannedTenderActivity> existingTenderDetails,
                                                                PlannedTenderForm form) {
    return new ModelAndView("scap/scap/plannedtender/plannedTenderActivityList")
        .addObject("form", form)
        .addObject("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(scapId)))
        .addObject("plannedTenderDetailsList",
            plannedTenderActivityListService.plannedTenderDetailsToListItems(scapId, existingTenderDetails))
        .addObject("radioItems", HasMorePlannedTenderActivities.getRadioItems());
  }
}
