package uk.co.nstauthority.scap.scap.plannedtender.activity.delete;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;
import uk.co.nstauthority.scap.endpointvalidation.annotations.HasAnyPermissionForScap;
import uk.co.nstauthority.scap.endpointvalidation.annotations.ScapHasStatus;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.plannedtender.PlannedTenderController;
import uk.co.nstauthority.scap.scap.plannedtender.PlannedTenderService;
import uk.co.nstauthority.scap.scap.plannedtender.activity.PlannedTenderActivity;
import uk.co.nstauthority.scap.scap.plannedtender.activity.PlannedTenderActivityService;
import uk.co.nstauthority.scap.scap.plannedtender.hasplannedtender.HasPlannedTenderController;
import uk.co.nstauthority.scap.scap.plannedtender.list.PlannedTenderActivityListItem;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.scap.ScapService;
import uk.co.nstauthority.scap.util.SuccessBannerUtil;

@Controller
@RequestMapping("{scapId}/planned-tender/{plannedTenderDetailId}/delete")
@HasAnyPermissionForScap(permissions = RolePermission.SUBMIT_SCAP)
@ScapHasStatus(permittedStatuses = ScapDetailStatus.DRAFT)
public class DeletePlannedTenderActivityController {

  private final ScapService scapService;
  private final ScapDetailService scapDetailService;
  private final PlannedTenderService plannedTenderService;
  private final PlannedTenderActivityService plannedTenderActivityService;

  @Autowired
  DeletePlannedTenderActivityController(ScapService scapService,
                                        ScapDetailService scapDetailService, PlannedTenderService plannedTenderService,
                                        PlannedTenderActivityService plannedTenderActivityService) {
    this.scapService = scapService;
    this.scapDetailService = scapDetailService;
    this.plannedTenderService = plannedTenderService;
    this.plannedTenderActivityService = plannedTenderActivityService;
  }

  @GetMapping
  public ModelAndView renderPlannedTenderRemoval(@PathVariable("scapId") ScapId scapId,
                                                 @PathVariable("plannedTenderDetailId") Integer plannedTenderDetailId) {
    scapService.getScapById(scapId);
    var plannedTenderActivity = plannedTenderActivityService.getPlannedTenderDetailById(plannedTenderDetailId);
    return plannedTenderRemovalModelAndView(scapId, plannedTenderActivity);
  }

  @PostMapping
  public ModelAndView deletePlannedTenderDetail(@PathVariable("scapId") ScapId scapId,
                                                @PathVariable("plannedTenderDetailId") Integer plannedTenderDetailId,
                                                RedirectAttributes redirectAttributes) {
    var scap = scapService.getScapById(scapId);
    var scapDetail = scapDetailService.getLatestByScap(scap);
    var plannedTender = plannedTenderService.getByScapDetail(scapDetail);
    var plannedTenderDetail = plannedTenderActivityService.getPlannedTenderDetailById(plannedTenderDetailId);
    plannedTenderActivityService.deletePlannedTenderDetail(plannedTenderDetail);

    SuccessBannerUtil.add(redirectAttributes, "Planned tender activity deleted successfully");

    if (plannedTenderActivityService.hasExistingTenderDetails(plannedTender)) {
      return ReverseRouter.redirect(on(PlannedTenderController.class).renderPlannedTenderActivities(scapId));
    }

    plannedTenderService.updatePlannedTenderHasMorePlannedTenders(plannedTender, null);
    return ReverseRouter.redirect(on(HasPlannedTenderController.class).renderHasPlannedTenderActivityForm(scapId));
  }

  private ModelAndView plannedTenderRemovalModelAndView(ScapId scapId, PlannedTenderActivity plannedTenderActivity) {
    return new ModelAndView("scap/scap/plannedtender/plannedTenderActivityDelete")
        .addObject("backLinkUrl",
            ReverseRouter.route(on(PlannedTenderController.class).renderPlannedTenderActivities(scapId)))
        .addObject("plannedTenderActivityView", PlannedTenderActivityListItem.from(scapId, plannedTenderActivity))
        .addObject("submitPostUrl",
            ReverseRouter.route(on(DeletePlannedTenderActivityController.class)
                .deletePlannedTenderDetail(scapId, plannedTenderActivity.getId(), null)));
  }
}
