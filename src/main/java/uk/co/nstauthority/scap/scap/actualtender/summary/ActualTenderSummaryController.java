package uk.co.nstauthority.scap.scap.actualtender.summary;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.List;
import javax.transaction.Transactional;
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
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.permissionmanagement.endpointsecurity.PermissionsRequiredForScap;
import uk.co.nstauthority.scap.scap.actualtender.ActualTenderControllerRedirectionService;
import uk.co.nstauthority.scap.scap.actualtender.ActualTenderService;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivityService;
import uk.co.nstauthority.scap.scap.actualtender.hasactualtender.HasActualTenderController;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.scap.ScapService;
import uk.co.nstauthority.scap.scap.summary.actualtender.ActualTenderActivitySummaryView;
import uk.co.nstauthority.scap.scap.summary.actualtender.ActualTenderSummaryViewService;
import uk.co.nstauthority.scap.scap.tasklist.TaskListController;

@Controller
@RequestMapping("{scapId}/actual-tender/summary")
@PermissionsRequiredForScap(permissions = RolePermission.SUBMIT_SCAP)
public class ActualTenderSummaryController {

  private final ScapService scapService;
  private final ScapDetailService scapDetailService;
  private final ActualTenderService actualTenderService;
  private final ActualTenderActivityService actualTenderActivityService;
  private final ActualTenderSummaryFormService actualTenderSummaryFormService;
  private final ControllerHelperService controllerHelperService;
  private final ActualTenderControllerRedirectionService actualTenderControllerRedirectionService;
  private final ActualTenderSummaryViewService actualTenderSummaryViewService;

  ActualTenderSummaryController(ScapService scapService, ScapDetailService scapDetailService,
                                ActualTenderService actualTenderService,
                                ActualTenderActivityService actualTenderActivityService,
                                ActualTenderSummaryFormService actualTenderSummaryFormService,
                                ControllerHelperService controllerHelperService,
                                ActualTenderControllerRedirectionService actualTenderControllerRedirectionService,
                                ActualTenderSummaryViewService actualTenderSummaryViewService) {
    this.scapService = scapService;
    this.scapDetailService = scapDetailService;
    this.actualTenderService = actualTenderService;
    this.actualTenderActivityService = actualTenderActivityService;
    this.actualTenderSummaryFormService = actualTenderSummaryFormService;
    this.controllerHelperService = controllerHelperService;
    this.actualTenderControllerRedirectionService = actualTenderControllerRedirectionService;
    this.actualTenderSummaryViewService = actualTenderSummaryViewService;
  }

  @GetMapping
  @Transactional
  public ModelAndView renderActualTenderSummary(@PathVariable("scapId") ScapId scapId) {
    var scap = scapService.getScapById(scapId);
    var scapDetail = scapDetailService.getLatestScapDetailByScapOrThrow(scap);
    var actualTender = actualTenderService.getByScapDetailOrThrow(scapDetail);
    var actualTenderActivities = actualTenderActivityService.getAllByActualTender(actualTender);

    if (actualTenderActivities.isEmpty()) {
      return ReverseRouter.redirect(on(HasActualTenderController.class).renderHasActualTenderForm(scapId));
    }

    var form = actualTenderSummaryFormService.getForm(actualTender);
    var actualTenderSummaryViews = actualTenderSummaryViewService
        .getByActualTenderActivities(actualTenderActivities, scapId);

    return actualTenderSummaryModelAndView(scapId, actualTenderSummaryViews)
        .addObject("form", form);
  }

  @PostMapping
  public ModelAndView saveActualTenderSummary(@PathVariable("scapId") ScapId scapId,
                                              @ModelAttribute("form") ActualTenderSummaryForm form,
                                              BindingResult bindingResult) {
    var scap = scapService.getScapById(scapId);
    var scapDetail = scapDetailService.getLatestScapDetailByScapOrThrow(scap);
    var actualTender = actualTenderService.getByScapDetailOrThrow(scapDetail);
    var actualTenderActivities = actualTenderActivityService.getAllByActualTender(actualTender);

    if (actualTenderActivities.isEmpty()) {
      return ReverseRouter.redirect(on(HasActualTenderController.class).renderHasActualTenderForm(scapId));
    }

    var actualTenderSummaryViews = actualTenderSummaryViewService
        .getByActualTenderActivities(actualTenderActivities, scapId);

    bindingResult = actualTenderSummaryFormService.validate(form, bindingResult);

    return controllerHelperService.checkErrorsAndRedirect(
        bindingResult,
        actualTenderSummaryModelAndView(scapId, actualTenderSummaryViews),
        form,
        () -> {
          var hasMoreActualTenderActivities = form.getHasMoreActualTenderActivities();
          actualTenderService.updateHasMoreActualTenders(actualTender, hasMoreActualTenderActivities);
          return actualTenderControllerRedirectionService.redirectFromActualTenderSummary(scapId,
              hasMoreActualTenderActivities);
        }
    );
  }

  private ModelAndView actualTenderSummaryModelAndView(ScapId scapId, List<ActualTenderActivitySummaryView> summaryViews) {
    return new ModelAndView("scap/scap/actualtender/actualTenderActivitySummary")
        .addObject("actualTenderActivities", summaryViews)
        .addObject("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(scapId)))
        .addObject("radioItems", HasMoreActualTenderActivities.getRadioItems());
  }
}
