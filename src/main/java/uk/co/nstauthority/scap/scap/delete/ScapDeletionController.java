package uk.co.nstauthority.scap.scap.delete;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;
import uk.co.nstauthority.scap.endpointvalidation.annotations.ScapHasStatus;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.permissionmanagement.endpointsecurity.PermissionsRequiredForScap;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.scap.ScapService;
import uk.co.nstauthority.scap.scap.summary.ScapSummaryController;
import uk.co.nstauthority.scap.scap.summary.ScapSummaryViewService;
import uk.co.nstauthority.scap.scap.tasklist.TaskListController;
import uk.co.nstauthority.scap.util.SuccessBannerUtil;
import uk.co.nstauthority.scap.workarea.WorkAreaController;

@Controller
@RequestMapping("{scapId}/delete")
@PermissionsRequiredForScap(permissions = RolePermission.SUBMIT_SCAP)
@ScapHasStatus(permittedStatuses = ScapDetailStatus.DRAFT)
public class ScapDeletionController {

  private final ScapService scapService;
  private final ScapDetailService scapDetailService;
  private final ScapSummaryViewService scapSummaryViewService;

  @Autowired
  public ScapDeletionController(ScapService scapService,
                                ScapDetailService scapDetailService,
                                ScapSummaryViewService scapSummaryViewService) {
    this.scapService = scapService;
    this.scapDetailService = scapDetailService;
    this.scapSummaryViewService = scapSummaryViewService;
  }

  @GetMapping
  public ModelAndView renderScapDeletionConfirmation(@PathVariable("scapId") ScapId scapId) {
    var scap = scapService.getScapById(scapId);
    var scapDetail = scapDetailService.getLatestScapDetailByScapOrThrow(scap);
    var scapSummaryView = scapSummaryViewService.getScapSummaryView(scapDetail);

    return new ModelAndView("scap/scap/deleteScap")
        .addObject("backLinkUrl", ReverseRouter.route(on(TaskListController.class).renderTaskList(scapId)))
        .addObject("scapSummaryView", scapSummaryView)
        .addObject("reference", scap.getReference());
  }

  @PostMapping
  ModelAndView deleteScap(@PathVariable("scapId") ScapId scapId,
                          RedirectAttributes redirectAttributes) {
    var reference = scapService.getScapById(scapId).getReference();
    var scapDetail = scapDetailService.getLatestScapDetailByScapIdOrThrow(scapId);
    scapDetailService.deleteScapDetail(scapDetail);

    var successMessage = "%s deleted successfully".formatted(reference);
    SuccessBannerUtil.add(redirectAttributes, successMessage);
    if (scapDetail.getVersionNumber() > 1) {
      return ReverseRouter.redirect(on(ScapSummaryController.class).getScapSummary(scapId));
    }
    return ReverseRouter.redirect(on(WorkAreaController.class).getWorkArea(null));
  }
}
