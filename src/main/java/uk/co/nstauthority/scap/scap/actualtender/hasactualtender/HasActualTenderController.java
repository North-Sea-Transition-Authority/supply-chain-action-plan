package uk.co.nstauthority.scap.scap.actualtender.hasactualtender;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.Optional;
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
import uk.co.nstauthority.scap.endpointvalidation.annotations.ScapHasStatus;
import uk.co.nstauthority.scap.enumutil.YesNo;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.permissionmanagement.endpointsecurity.PermissionsRequiredForScap;
import uk.co.nstauthority.scap.scap.actualtender.ActualTender;
import uk.co.nstauthority.scap.scap.actualtender.ActualTenderService;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivityController;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivityService;
import uk.co.nstauthority.scap.scap.actualtender.summary.ActualTenderSummaryController;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.scap.ScapService;
import uk.co.nstauthority.scap.scap.tasklist.TaskListController;

@Controller
@RequestMapping("{scapId}/actual-tender")
@PermissionsRequiredForScap(permissions = RolePermission.SUBMIT_SCAP)
@ScapHasStatus(permittedStatuses = ScapDetailStatus.DRAFT)
public class HasActualTenderController {

  private final ScapService scapService;
  private final ScapDetailService scapDetailService;
  private final ActualTenderService actualTenderService;
  private final ControllerHelperService controllerHelperService;
  private final HasActualTenderFormService hasActualTenderFormService;
  private final ActualTenderActivityService actualTenderActivityService;

  @Autowired
  HasActualTenderController(ScapService scapService, ScapDetailService scapDetailService,
                            ActualTenderService actualTenderService, ControllerHelperService controllerHelperService,
                            HasActualTenderFormService hasActualTenderFormService,
                            ActualTenderActivityService actualTenderActivityService) {
    this.scapService = scapService;
    this.scapDetailService = scapDetailService;
    this.actualTenderService = actualTenderService;
    this.controllerHelperService = controllerHelperService;
    this.hasActualTenderFormService = hasActualTenderFormService;
    this.actualTenderActivityService = actualTenderActivityService;
  }

  @GetMapping
  public ModelAndView renderHasActualTenderForm(@PathVariable("scapId") ScapId scapId) {
    var scap = scapService.getScapById(scapId);
    var scapDetail = scapDetailService.getLatestByScap(scap);
    var actualTender = actualTenderService.findByScapDetail(scapDetail);
    var form = actualTender
        .map(hasActualTenderFormService::getForm)
        .orElse(new HasActualTenderForm());
    if (hasExistingActualTenderActivities(actualTender)) {
      return ReverseRouter.redirect(on(ActualTenderSummaryController.class).renderActualTenderSummary(scapId));
    }

    return hasActualTenderFormModelAndView(scapId, form);
  }

  @PostMapping
  ModelAndView saveHasActualTenderForm(@PathVariable("scapId") ScapId scapId,
                                       @ModelAttribute("form") HasActualTenderForm form,
                                       BindingResult bindingResult) {
    var scap = scapService.getScapById(scapId);
    var scapDetail = scapDetailService.getLatestByScap(scap);
    var actualTender = actualTenderService.findByScapDetail(scapDetail);

    if (hasExistingActualTenderActivities(actualTender)) {
      return ReverseRouter.redirect(on(ActualTenderSummaryController.class).renderActualTenderSummary(scapId));
    }

    bindingResult = hasActualTenderFormService.validate(form, bindingResult);

    return controllerHelperService.checkErrorsAndRedirect(
        bindingResult,
        hasActualTenderFormModelAndView(scapId, form),
        form,
        () -> {
          actualTender.ifPresentOrElse(
              existingActualTender -> actualTenderService.updateHasActualTenders(
                  existingActualTender,
                  YesNo.YES.equals(form.getHasActualTender())),
              () -> actualTenderService.createActualTender(scapDetail, form.getHasActualTender())
          );
          if (YesNo.YES.equals(form.getHasActualTender())) {
            return ReverseRouter.redirect(on(ActualTenderActivityController.class)
                .renderActualTenderActivityForm(scapId, null));
          }
          return ReverseRouter.redirect(on(TaskListController.class).renderTaskList(scapId));
        });
  }

  private ModelAndView hasActualTenderFormModelAndView(ScapId scapId, HasActualTenderForm form) {
    return new ModelAndView("scap/scap/actualtender/hasActualTender")
        .addObject("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(scapId)))
        .addObject("hasActualTender", YesNo.getRadioOptions())
        .addObject("form", form);
  }

  private boolean hasExistingActualTenderActivities(Optional<ActualTender> actualTender) {
    return actualTender.isPresent() && actualTenderActivityService.hasActualTenderActivity(actualTender.get());
  }
}
