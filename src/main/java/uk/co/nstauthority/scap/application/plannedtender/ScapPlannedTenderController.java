package uk.co.nstauthority.scap.application.plannedtender;

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
import uk.co.nstauthority.scap.application.detail.ScapDetailService;
import uk.co.nstauthority.scap.application.overview.ScapOverviewService;
import uk.co.nstauthority.scap.application.plannedtender.detail.ScapPlannedTenderDetail;
import uk.co.nstauthority.scap.application.plannedtender.detail.ScapPlannedTenderDetailController;
import uk.co.nstauthority.scap.application.plannedtender.detail.ScapPlannedTenderDetailService;
import uk.co.nstauthority.scap.application.plannedtender.hasplannedtender.ScapHasPlannedTenderController;
import uk.co.nstauthority.scap.application.plannedtender.list.PlannedTenderDetailListService;
import uk.co.nstauthority.scap.application.tasklist.TaskListController;
import uk.co.nstauthority.scap.controllerhelper.ControllerHelperService;
import uk.co.nstauthority.scap.mvc.ReverseRouter;

@Controller
@RequestMapping("{scapId}/planned-tender/activity-summary")
public class ScapPlannedTenderController {

  private final ScapOverviewService scapOverviewService;
  private final ScapDetailService scapDetailService;
  private final ScapPlannedTenderService scapPlannedTenderService;
  private final ScapPlannedTenderDetailService scapPlannedTenderDetailService;
  private final PlannedTenderDetailListService plannedTenderDetailListService;
  private final ScapPlannedTenderFormService scapPlannedTenderFormService;
  private final ControllerHelperService controllerHelperService;

  public ScapPlannedTenderController(ScapOverviewService scapOverviewService, ScapDetailService scapDetailService,
                                     ScapPlannedTenderService scapPlannedTenderService,
                                     ScapPlannedTenderDetailService scapPlannedTenderDetailService,
                                     PlannedTenderDetailListService plannedTenderDetailListService,
                                     ScapPlannedTenderFormService scapPlannedTenderFormService,
                                     ControllerHelperService controllerHelperService) {
    this.scapOverviewService = scapOverviewService;
    this.scapDetailService = scapDetailService;
    this.scapPlannedTenderService = scapPlannedTenderService;
    this.scapPlannedTenderDetailService = scapPlannedTenderDetailService;
    this.plannedTenderDetailListService = plannedTenderDetailListService;
    this.scapPlannedTenderFormService = scapPlannedTenderFormService;
    this.controllerHelperService = controllerHelperService;
  }

  @GetMapping
  public ModelAndView renderPlannedTenderActivities(@PathVariable("scapId") Integer scapId) {
    var scap = scapOverviewService.getScapById(scapId);
    var scapDetail = scapDetailService.getLatestScapDetailByScapOrThrow(scap);
    var plannedTender = scapPlannedTenderService.getScapPlannedTenderByScapDetailOrThrow(scapDetail);
    var existingTenderDetails = scapPlannedTenderDetailService.getTenderDetailsByPlannedTender(plannedTender);
    var form = scapPlannedTenderFormService.getForm(plannedTender);

    if (existingTenderDetails.isEmpty()) {
      return ReverseRouter.redirect(on(ScapHasPlannedTenderController.class).renderHasPlannedTenderActivityForm(scapId));
    }

    return plannedTenderActivitySummaryModelAndView(scapId, existingTenderDetails, form);
  }

  @PostMapping
  public ModelAndView saveAnotherPlannedTenderActivity(@PathVariable("scapId") Integer scapId,
                                                       @ModelAttribute("form") ScapPlannedTenderForm form,
                                                       BindingResult bindingResult) {
    var scap = scapOverviewService.getScapById(scapId);
    var scapDetail = scapDetailService.getLatestScapDetailByScapOrThrow(scap);
    var plannedTender = scapPlannedTenderService.getScapPlannedTenderByScapDetailOrThrow(scapDetail);
    var existingTenderDetails = scapPlannedTenderDetailService.getTenderDetailsByPlannedTender(plannedTender);
    bindingResult = scapPlannedTenderFormService.validate(form, bindingResult);

    return controllerHelperService.checkErrorsAndRedirect(
        bindingResult,
        plannedTenderActivitySummaryModelAndView(scapId, existingTenderDetails, form),
        form,
        () -> {
          if (HasMorePlannedTenderActivities.YES_NOW.equals(form.getHasMorePlannedTenderActivities())) {
            return ReverseRouter.redirect(on(ScapPlannedTenderDetailController.class)
                .renderPlannedTenderDetailForm(scapId, null));
          }

          scapPlannedTenderService
              .updatePlannedTenderHasMorePlannedTenders(plannedTender, form.getHasMorePlannedTenderActivities());

          return ReverseRouter.redirect(on(TaskListController.class).renderTaskList(scapId));
        });
  }

  private ModelAndView plannedTenderActivitySummaryModelAndView(Integer scapId,
                                                                List<ScapPlannedTenderDetail> existingTenderDetails,
                                                                ScapPlannedTenderForm form) {
    return new ModelAndView("scap/application/plannedtender/plannedTenderActivityList")
        .addObject("form", form)
        .addObject("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(scapId)))
        .addObject("plannedTenderDetailsList",
            plannedTenderDetailListService.plannedTenderDetailsToListItems(scapId, existingTenderDetails))
        .addObject("radioItems", HasMorePlannedTenderActivities.getRadioItems());
  }
}
