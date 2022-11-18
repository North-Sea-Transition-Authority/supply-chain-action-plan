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
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.plannedtender.PlannedTenderController;
import uk.co.nstauthority.scap.scap.plannedtender.PlannedTenderService;
import uk.co.nstauthority.scap.scap.plannedtender.activity.PlannedTenderActivity;
import uk.co.nstauthority.scap.scap.plannedtender.activity.PlannedTenderActivityService;
import uk.co.nstauthority.scap.scap.plannedtender.hasplannedtender.HasPlannedTenderController;
import uk.co.nstauthority.scap.scap.scap.ScapService;
import uk.co.nstauthority.scap.util.DeletionSuccessBannerUtil;

@Controller
@RequestMapping("{scapId}/planned-tender/{plannedTenderDetailId}/delete")
public class DeletePlannedTenderDetailController {

  private final ScapService scapService;
  private final ScapDetailService scapDetailService;
  private final PlannedTenderService plannedTenderService;
  private final PlannedTenderActivityService plannedTenderActivityService;

  @Autowired
  DeletePlannedTenderDetailController(ScapService scapService,
                                      ScapDetailService scapDetailService, PlannedTenderService plannedTenderService,
                                      PlannedTenderActivityService plannedTenderActivityService) {
    this.scapService = scapService;
    this.scapDetailService = scapDetailService;
    this.plannedTenderService = plannedTenderService;
    this.plannedTenderActivityService = plannedTenderActivityService;
  }

  @GetMapping
  public ModelAndView renderPlannedTenderRemoval(@PathVariable("scapId") Integer scapId,
                                                 @PathVariable("plannedTenderDetailId") Integer plannedTenderDetailId) {
    scapService.getScapById(scapId);
    var plannedTenderDetail = plannedTenderActivityService.getPlannedTenderDetailById(plannedTenderDetailId);
    return plannedTenderRemovalModelAndView(scapId, plannedTenderDetail);
  }

  @PostMapping
  public ModelAndView deletePlannedTenderDetail(@PathVariable("scapId") Integer scapId,
                                                @PathVariable("plannedTenderDetailId") Integer plannedTenderDetailId,
                                                RedirectAttributes redirectAttributes) {
    var scap = scapService.getScapById(scapId);
    var scapDetail = scapDetailService.getLatestScapDetailByScapOrThrow(scap);
    var plannedTender = plannedTenderService.getScapPlannedTenderByScapDetailOrThrow(scapDetail);
    var plannedTenderDetail = plannedTenderActivityService.getPlannedTenderDetailById(plannedTenderDetailId);
    plannedTenderActivityService.deletePlannedTenderDetail(plannedTenderDetail);

    DeletionSuccessBannerUtil.addRedirectionNotification(redirectAttributes, "Planned tender activity deleted successfully");

    if (plannedTenderActivityService.hasExistingTenderDetails(plannedTender)) {
      return ReverseRouter.redirect(on(PlannedTenderController.class).renderPlannedTenderActivities(scapId));
    }

    return ReverseRouter.redirect(on(HasPlannedTenderController.class).renderHasPlannedTenderActivityForm(scapId));
  }

  private ModelAndView plannedTenderRemovalModelAndView(Integer scapId, PlannedTenderActivity plannedTenderDetail) {
    return new ModelAndView("scap/scap/plannedtender/plannedTenderActivityDelete")
        .addObject("backLinkUrl",
            ReverseRouter.route(on(PlannedTenderController.class).renderPlannedTenderActivities(scapId)))
        .addObject("plannedTenderDetail", plannedTenderDetail)
        .addObject("submitPostUrl",
            ReverseRouter.route(on(DeletePlannedTenderDetailController.class)
                .deletePlannedTenderDetail(scapId, plannedTenderDetail.getId(), null)));
  }
}
