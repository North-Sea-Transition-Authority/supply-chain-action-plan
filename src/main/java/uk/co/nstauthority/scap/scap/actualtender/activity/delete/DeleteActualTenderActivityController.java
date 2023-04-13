package uk.co.nstauthority.scap.scap.actualtender.activity.delete;

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
import uk.co.nstauthority.scap.scap.actualtender.ActualTenderControllerRedirectionService;
import uk.co.nstauthority.scap.scap.actualtender.ActualTenderService;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivity;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivityService;
import uk.co.nstauthority.scap.scap.actualtender.summary.ActualTenderSummaryController;
import uk.co.nstauthority.scap.scap.contractingperformance.ContractingPerformanceService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.scap.ScapService;
import uk.co.nstauthority.scap.scap.summary.actualtender.ActualTenderSummaryViewService;
import uk.co.nstauthority.scap.util.SuccessBannerUtil;

@Controller
@RequestMapping("{scapId}/actual-tender/activity/{activityId}/delete")
@PermissionsRequiredForScap(permissions = RolePermission.SUBMIT_SCAP)
@ScapHasStatus(permittedStatuses = ScapDetailStatus.DRAFT)
public class DeleteActualTenderActivityController {

  static final String DELETES_CONTRACTING_PERFORMANCE_WARNING =
      "This actual tendering activity has associated contracting performance. " +
      "The related contracting performance information will also be deleted.";
  private final ScapService scapService;
  private final ScapDetailService scapDetailService;
  private final ActualTenderService actualTenderService;
  private final ActualTenderActivityService actualTenderActivityService;
  private final ActualTenderSummaryViewService actualTenderSummaryViewService;
  private final DeleteActualTenderActivityService deleteActualTenderActivityService;
  private final ActualTenderControllerRedirectionService actualTenderControllerRedirectionService;
  private final ContractingPerformanceService contractingPerformanceService;

  @Autowired
  DeleteActualTenderActivityController(ScapService scapService, ScapDetailService scapDetailService,
                                       ActualTenderService actualTenderService,
                                       ActualTenderActivityService actualTenderActivityService,
                                       ActualTenderSummaryViewService actualTenderSummaryViewService,
                                       DeleteActualTenderActivityService deleteActualTenderActivityService,
                                       ActualTenderControllerRedirectionService actualTenderControllerRedirectionService,
                                       ContractingPerformanceService contractingPerformanceService) {
    this.scapService = scapService;
    this.scapDetailService = scapDetailService;
    this.actualTenderService = actualTenderService;
    this.actualTenderActivityService = actualTenderActivityService;
    this.actualTenderSummaryViewService = actualTenderSummaryViewService;
    this.deleteActualTenderActivityService = deleteActualTenderActivityService;
    this.actualTenderControllerRedirectionService = actualTenderControllerRedirectionService;
    this.contractingPerformanceService = contractingPerformanceService;
  }

  @GetMapping
  public ModelAndView renderDeleteActualTenderActivityConfirmation(@PathVariable("scapId") ScapId scapId,
                                                                   @PathVariable("activityId") Integer activityId) {
    scapService.getScapById(scapId);
    var actualTenderActivity = actualTenderActivityService.getById(activityId);
    var actualTenderActivityView = actualTenderSummaryViewService
        .getSingleViewByActualTenderActivity(actualTenderActivity, scapId);

    var contractingPerformanceWarning = getContractingPerformanceWarning(actualTenderActivity);

    return new ModelAndView("scap/scap/actualtender/deleteActualTender")
        .addObject("backLinkUrl", ReverseRouter.route(on(ActualTenderSummaryController.class)
            .renderActualTenderSummary(scapId)))
        .addObject("actualTenderActivityView", actualTenderActivityView)
        .addObject("contractingPerformanceWarning", contractingPerformanceWarning);
  }

  @PostMapping
  public ModelAndView submitDeleteActualTenderActivity(@PathVariable("scapId") ScapId scapId,
                                                       @PathVariable("activityId") Integer activityId,
                                                       RedirectAttributes redirectAttributes) {
    var scap = scapService.getScapById(scapId);
    var scapDetail = scapDetailService.getLatestScapDetailByScapOrThrow(scap);
    var actualTender = actualTenderService.getByScapDetail(scapDetail);
    var actualTenderActivity = actualTenderActivityService.getById(activityId);
    deleteActualTenderActivityService.deleteActualTenderActivity(actualTenderActivity);

    SuccessBannerUtil.add(
        redirectAttributes,
        "%s has been removed from this SCAP".formatted(actualTenderActivity.getScopeTitle())
    );

    var hasRemainingActualTenderActivities = actualTenderActivityService.hasActualTenderActivity(actualTender);

    if (!hasRemainingActualTenderActivities) {
      actualTenderService.updateHasMoreActualTenders(actualTender, null);
    }
    return actualTenderControllerRedirectionService.redirectFromActualTenderDeletion(scapId, hasRemainingActualTenderActivities);
  }

  private String getContractingPerformanceWarning(ActualTenderActivity actualTenderActivity) {
    if (contractingPerformanceService.hasContractingPerformance(actualTenderActivity)) {
      return DELETES_CONTRACTING_PERFORMANCE_WARNING;
    }
    return null;
  }
}
