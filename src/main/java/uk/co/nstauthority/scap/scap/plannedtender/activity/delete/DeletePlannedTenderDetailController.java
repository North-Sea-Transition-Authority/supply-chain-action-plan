package uk.co.nstauthority.scap.scap.plannedtender.activity.delete;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.plannedtender.PlannedTenderController;
import uk.co.nstauthority.scap.scap.plannedtender.activity.PlannedTenderActivity;
import uk.co.nstauthority.scap.scap.plannedtender.activity.PlannedTenderActivityService;
import uk.co.nstauthority.scap.scap.scap.ScapService;

@Controller
@RequestMapping("{scapId}/planned-tender/{plannedTenderDetailId}/delete")
public class DeletePlannedTenderDetailController {

  private final ScapService scapService;
  private final PlannedTenderActivityService plannedTenderActivityService;

  @Autowired
  DeletePlannedTenderDetailController(ScapService scapService,
                                      PlannedTenderActivityService plannedTenderActivityService) {
    this.scapService = scapService;
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
                                                @PathVariable("plannedTenderDetailId") Integer plannedTenderDetailId) {
    scapService.getScapById(scapId);
    var plannedTenderDetail = plannedTenderActivityService.getPlannedTenderDetailById(plannedTenderDetailId);
    plannedTenderActivityService.deletePlannedTenderDetail(plannedTenderDetail);

    return ReverseRouter.redirect(on(PlannedTenderController.class).renderPlannedTenderActivities(scapId));
  }

  private ModelAndView plannedTenderRemovalModelAndView(Integer scapId, PlannedTenderActivity plannedTenderDetail) {
    return new ModelAndView("scap/application/plannedtender/plannedTenderActivityDelete")
        .addObject("backLinkUrl",
            ReverseRouter.route(on(PlannedTenderController.class).renderPlannedTenderActivities(scapId)))
        .addObject("plannedTenderDetail", plannedTenderDetail)
        .addObject("submitPostUrl",
            ReverseRouter.route(on(DeletePlannedTenderDetailController.class)
                .deletePlannedTenderDetail(scapId, plannedTenderDetail.getId())));
  }
}
