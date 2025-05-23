package uk.co.nstauthority.scap.scap.plannedtender.activity;

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
import uk.co.nstauthority.scap.controllerhelper.ControllerHelperService;
import uk.co.nstauthority.scap.endpointvalidation.annotations.HasAnyPermissionForScap;
import uk.co.nstauthority.scap.endpointvalidation.annotations.ScapHasStatus;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.scap.RemunerationModel;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.plannedtender.PlannedTender;
import uk.co.nstauthority.scap.scap.plannedtender.PlannedTenderController;
import uk.co.nstauthority.scap.scap.plannedtender.PlannedTenderService;
import uk.co.nstauthority.scap.scap.plannedtender.hasplannedtender.HasPlannedTenderController;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.scap.ScapService;

@Controller
@RequestMapping("{scapId}/planned-tender/activity")
@HasAnyPermissionForScap(permissions = RolePermission.SUBMIT_SCAP)
@ScapHasStatus(permittedStatuses = ScapDetailStatus.DRAFT)
public class PlannedTenderActivityController {

  private final ScapService scapService;
  private final ScapDetailService scapDetailService;
  private final PlannedTenderService plannedTenderService;
  private final PlannedTenderActivityService plannedTenderActivityService;
  private final PlannedTenderActivityFormService plannedTenderActivityFormService;
  private final ControllerHelperService controllerHelperService;

  @Autowired
  public PlannedTenderActivityController(ScapService scapService, ScapDetailService scapDetailService,
                                         PlannedTenderService plannedTenderService,
                                         PlannedTenderActivityService plannedTenderActivityService,
                                         PlannedTenderActivityFormService plannedTenderActivityFormService,
                                         ControllerHelperService controllerHelperService) {
    this.scapService = scapService;
    this.scapDetailService = scapDetailService;
    this.plannedTenderService = plannedTenderService;
    this.plannedTenderActivityService = plannedTenderActivityService;
    this.plannedTenderActivityFormService = plannedTenderActivityFormService;
    this.controllerHelperService = controllerHelperService;
  }

  @GetMapping
  public ModelAndView renderPlannedTenderDetailForm(@PathVariable("scapId") ScapId scapId,
                                                    @ModelAttribute("form") PlannedTenderActivityForm form) {
    var scap = scapService.getScapById(scapId);
    var scapDetail = scapDetailService.getLatestByScap(scap);
    var scapPlannedTender = plannedTenderService.getByScapDetail(scapDetail);

    return plannedTenderDetailFormModelAndView(scapId, getBackLinkUrl(scapId, scapPlannedTender));

  }

  @PostMapping
  public ModelAndView savePlannedTenderDetailForm(@PathVariable("scapId") ScapId scapId,
                                                  @ModelAttribute("form") PlannedTenderActivityForm form,
                                                  BindingResult bindingResult) {
    bindingResult = plannedTenderActivityFormService.validate(bindingResult, form);
    var scap = scapService.getScapById(scapId);
    var scapDetail = scapDetailService.getLatestByScap(scap);
    var scapPlannedTender = plannedTenderService.getByScapDetail(scapDetail);

    return controllerHelperService.checkErrorsAndRedirect(
        bindingResult,
        plannedTenderDetailFormModelAndView(scapId, getBackLinkUrl(scapId, scapPlannedTender)),
        form,
        () -> {
          plannedTenderActivityService.createPlannedTenderDetail(scapPlannedTender, form);

          return ReverseRouter.redirect(on(PlannedTenderController.class).renderPlannedTenderActivities(scapId));
        }
    );
  }

  private String getBackLinkUrl(ScapId scapId, PlannedTender plannedTender) {
    if (plannedTenderActivityService.hasExistingTenderDetails(plannedTender)) {
      return ReverseRouter.route(on(PlannedTenderController.class).renderPlannedTenderActivities(scapId));
    }
    return ReverseRouter.route(on(HasPlannedTenderController.class).renderHasPlannedTenderActivityForm(scapId));
  }

  private ModelAndView plannedTenderDetailFormModelAndView(ScapId scapId, String backLinkUrl) {
    return new ModelAndView("scap/scap/plannedtender/plannedTenderActivityDetails")
        .addObject("backLinkUrl", backLinkUrl)
        .addObject("submitPostUrl",
            ReverseRouter.route(on(PlannedTenderActivityController.class)
                .savePlannedTenderDetailForm(scapId, null, emptyBindingResult())))
        .addObject("remunerationModels", RemunerationModel.getRemunerationModels());
  }
}
