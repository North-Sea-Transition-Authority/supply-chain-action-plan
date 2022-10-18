package uk.co.nstauthority.scap.application.plannedtender.detail.delete;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;
import uk.co.nstauthority.scap.application.overview.ScapOverviewService;
import uk.co.nstauthority.scap.application.plannedtender.ScapPlannedTenderController;
import uk.co.nstauthority.scap.application.plannedtender.detail.ScapPlannedTenderDetail;
import uk.co.nstauthority.scap.application.plannedtender.detail.ScapPlannedTenderDetailService;
import uk.co.nstauthority.scap.mvc.ReverseRouter;

@Controller
@RequestMapping("{scapId}/planned-tender/{plannedTenderDetailId}/delete")
public class DeletePlannedTenderDetailController {

  private final ScapOverviewService scapOverviewService;
  private final ScapPlannedTenderDetailService scapPlannedTenderDetailService;

  @Autowired
  DeletePlannedTenderDetailController(ScapOverviewService scapOverviewService,
                                      ScapPlannedTenderDetailService scapPlannedTenderDetailService) {
    this.scapOverviewService = scapOverviewService;
    this.scapPlannedTenderDetailService = scapPlannedTenderDetailService;
  }

  @GetMapping
  public ModelAndView renderPlannedTenderRemoval(@PathVariable("scapId") Integer scapId,
                                                 @PathVariable("plannedTenderDetailId") Integer plannedTenderDetailId) {
    scapOverviewService.getScapById(scapId);
    var plannedTenderDetail = scapPlannedTenderDetailService.getPlannedTenderDetailById(plannedTenderDetailId);
    return plannedTenderRemovalModelAndView(scapId, plannedTenderDetail);
  }

  @PostMapping
  public ModelAndView deletePlannedTenderDetail(@PathVariable("scapId") Integer scapId,
                                                @PathVariable("plannedTenderDetailId") Integer plannedTenderDetailId) {
    scapOverviewService.getScapById(scapId);
    var plannedTenderDetail = scapPlannedTenderDetailService.getPlannedTenderDetailById(plannedTenderDetailId);
    scapPlannedTenderDetailService.deletePlannedTenderDetail(plannedTenderDetail);

    return ReverseRouter.redirect(on(ScapPlannedTenderController.class).renderPlannedTenderActivities(scapId));
  }

  private ModelAndView plannedTenderRemovalModelAndView(Integer scapId, ScapPlannedTenderDetail plannedTenderDetail) {
    return new ModelAndView("scap/application/plannedTender/plannedTenderActivityDelete")
        .addObject("backLinkUrl",
            ReverseRouter.route(on(ScapPlannedTenderController.class).renderPlannedTenderActivities(scapId)))
        .addObject("plannedTenderDetail", plannedTenderDetail)
        .addObject("submitPostUrl",
            ReverseRouter.route(on(DeletePlannedTenderDetailController.class)
                .deletePlannedTenderDetail(scapId, plannedTenderDetail.getId())));
  }
}
