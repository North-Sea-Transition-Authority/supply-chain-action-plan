package uk.co.nstauthority.scap.scap.summary;

import com.google.common.annotations.VisibleForTesting;
import java.util.Collections;
import java.util.Objects;
import javax.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.fivium.formlibrary.validator.date.DateUtils;
import uk.co.nstauthority.scap.enumutil.YesNo;
import uk.co.nstauthority.scap.scap.actualtender.ActualTenderService;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivityService;
import uk.co.nstauthority.scap.scap.contractingperformance.ContractingPerformanceOverviewService;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.plannedtender.PlannedTenderService;
import uk.co.nstauthority.scap.scap.plannedtender.activity.PlannedTenderActivityService;
import uk.co.nstauthority.scap.scap.projectdetails.ProjectDetailsService;
import uk.co.nstauthority.scap.scap.projectperformance.ProjectPerformanceService;
import uk.co.nstauthority.scap.scap.summary.actualtender.ActualTenderSummaryView;
import uk.co.nstauthority.scap.scap.summary.actualtender.ActualTenderSummaryViewService;
import uk.co.nstauthority.scap.scap.summary.contractingperformance.ContractingPerformanceOverviewSummaryView;
import uk.co.nstauthority.scap.scap.summary.contractingperformance.ContractingPerformanceSummaryViewService;
import uk.co.nstauthority.scap.scap.summary.plannedtender.PlannedTenderActivitySummaryView;
import uk.co.nstauthority.scap.scap.summary.plannedtender.PlannedTenderSummaryView;

@Service
public class ScapSummaryViewService {

  public static final String  PROJECT_DETAILS_OBJECT_NAME = "projectDetailsSummaryView";
  public static final String PLANNED_TENDER_OBJECT_NAME = "plannedTenderSummaryView";

  public static final String ACTUAL_TENDER_OBJECT_NAME = "actualTenderSummaryView";

  private final ProjectDetailsService projectDetailsService;
  private final PlannedTenderService plannedTenderService;
  private final PlannedTenderActivityService plannedTenderActivityService;
  private final ActualTenderSummaryViewService actualTenderSummaryViewService;
  private final ActualTenderService actualTenderService;
  private final ActualTenderActivityService actualTenderActivityService;
  private final ContractingPerformanceOverviewService contractingPerformanceOverviewService;
  private final ContractingPerformanceSummaryViewService contractingPerformanceSummaryViewService;
  private final ProjectPerformanceService projectPerformanceService;

  @Autowired
  ScapSummaryViewService(ProjectDetailsService projectDetailsService,
                         PlannedTenderService plannedTenderService,
                         PlannedTenderActivityService plannedTenderActivityService,
                         ActualTenderSummaryViewService actualTenderSummaryViewService,
                         ActualTenderService actualTenderService,
                         ActualTenderActivityService actualTenderActivityService,
                         ContractingPerformanceOverviewService contractingPerformanceOverviewService,
                         ContractingPerformanceSummaryViewService contractingPerformanceSummaryViewService,
                         ProjectPerformanceService projectPerformanceService) {
    this.projectDetailsService = projectDetailsService;
    this.plannedTenderService = plannedTenderService;
    this.plannedTenderActivityService = plannedTenderActivityService;
    this.actualTenderSummaryViewService = actualTenderSummaryViewService;
    this.actualTenderService = actualTenderService;
    this.actualTenderActivityService = actualTenderActivityService;
    this.contractingPerformanceOverviewService = contractingPerformanceOverviewService;
    this.contractingPerformanceSummaryViewService = contractingPerformanceSummaryViewService;
    this.projectPerformanceService = projectPerformanceService;
  }

  @Transactional
  public ScapSummaryView getScapSummaryView(ScapDetail scapDetail) {
    var projectDetailsSummaryView = getProjectDetailsSummaryView(scapDetail);
    var plannedTenderSummaryView = getPlannedTenderSummaryView(scapDetail);
    var actualTenderSummaryView = getActualTenderSummaryView(scapDetail);
    var contractingPerformanceOverviewSummaryView = getContractingPerformanceOverviewSummaryView(scapDetail);
    var projectPerformanceSummaryView = getProjectPerformanceSummaryView(scapDetail);

    return new ScapSummaryView(
        projectDetailsSummaryView, plannedTenderSummaryView, actualTenderSummaryView,
        contractingPerformanceOverviewSummaryView, projectPerformanceSummaryView
    );
  }

  @VisibleForTesting
  public ProjectDetailsSummaryView getProjectDetailsSummaryView(ScapDetail scapDetail) {
    var projectDetailsOpt = projectDetailsService.getProjectDetails(scapDetail);
    return projectDetailsOpt.map(projectDetails -> {
      var projectTypes = projectDetailsService.getProjectTypesByProjectDetails(projectDetails)
          .stream().toList();
      var projectFacilities = projectDetailsService.getProjectFacilityNames(projectDetails);
      var hasFacilities = YesNo.fromBoolean(projectDetails.getHasFacilities());

      return new ProjectDetailsSummaryView(
          projectDetails.getProjectName(),
          projectTypes,
          projectDetails.getProjectCostEstimate(),
          projectDetails.getEstimatedValueLocalContent(),
          projectDetails.getFieldName(),
          hasFacilities,
          projectFacilities,
          DateUtils.format(projectDetails.getPlannedExecutionStartDate()),
          DateUtils.format(projectDetails.getPlannedCompletionDate())
      );
    }).orElse(new ProjectDetailsSummaryView(
        null, Collections.emptyList(), null, null, null,
        null, Collections.emptyList(), null, null
    ));
  }

  @VisibleForTesting
  public PlannedTenderSummaryView getPlannedTenderSummaryView(ScapDetail scapDetail) {
    var plannedTenderOpt = plannedTenderService.getScapPlannedTenderByScapDetail(scapDetail);
    return plannedTenderOpt.map(
        plannedTender -> {
          if (Boolean.FALSE.equals(plannedTender.getHasPlannedTenders())) {
            return new PlannedTenderSummaryView(false, Collections.emptyList());
          }

          var plannedTenderActivities = plannedTenderActivityService.getTenderDetailsByPlannedTender(plannedTender);
          var plannedTenderActivitySummaryViews = plannedTenderActivities.stream()
              .map(PlannedTenderActivitySummaryView::from)
              .toList();
          return new PlannedTenderSummaryView(plannedTender.getHasPlannedTenders(), plannedTenderActivitySummaryViews);
        }
    ).orElse(new PlannedTenderSummaryView(null, null));
  }

  @VisibleForTesting
  public ActualTenderSummaryView getActualTenderSummaryView(ScapDetail scapDetail) {
    var actualTenderOpt = actualTenderService.getByScapDetail(scapDetail);
    return actualTenderOpt.map(actualTender -> {
      if (Boolean.FALSE.equals(actualTender.getHasActualTenders())) {
        return new ActualTenderSummaryView(false, Collections.emptyList());
      }

      var actualTenderActivities = actualTenderActivityService.getAllByActualTender(actualTender);
      return new ActualTenderSummaryView(
          true,
          actualTenderSummaryViewService.getByActualTenderActivities(actualTenderActivities, scapDetail.getScap().getId()));
    }).orElse(new ActualTenderSummaryView(null, null));
  }

  @VisibleForTesting
  public ContractingPerformanceOverviewSummaryView getContractingPerformanceOverviewSummaryView(ScapDetail scapDetail) {
    var contractingPerformanceOverviewOpt = contractingPerformanceOverviewService.getByScapDetail(scapDetail);
    return contractingPerformanceOverviewOpt.map(contractingPerformanceOverview -> {
      if (Boolean.FALSE.equals(contractingPerformanceOverview.getHasContractingPerformance())) {
        return new ContractingPerformanceOverviewSummaryView(false, Collections.emptyList());
      }
      var contractingPerformanceSummaryViews = contractingPerformanceSummaryViewService
          .getContractingPerformanceSummaryViews(scapDetail.getScap().getScapId());
      return new ContractingPerformanceOverviewSummaryView(true, contractingPerformanceSummaryViews);
    }
    ).orElse(new ContractingPerformanceOverviewSummaryView(null, null));
  }

  @VisibleForTesting
  public ProjectPerformanceSummaryView getProjectPerformanceSummaryView(ScapDetail scapDetail) {
    var projectPerformanceOpt = projectPerformanceService.findByScapDetail(scapDetail);
    return projectPerformanceOpt.map(projectPerformance -> new ProjectPerformanceSummaryView(
        projectPerformance.getProjectCompleted(),
        DateUtils.format(projectPerformance.getStartDate()),
        DateUtils.format(projectPerformance.getCompletionDate()),
        projectPerformance.getOutturnCost()
        ))
        .orElse(new ProjectPerformanceSummaryView(null, null, null, null));
  }

  public ScapSubmissionStage inferSubmissionStatusFromSummary(ScapSummaryView scapSummaryView) {
    if (Boolean.TRUE.equals(scapSummaryView.projectPerformanceSummaryView().isProjectCompleted())) {
      return ScapSubmissionStage.PROJECT_COMPLETED;
    }
    if (Boolean.TRUE.equals(scapSummaryView.contractingPerformanceOverviewSummaryView().hasContractingPerformance())) {
      return ScapSubmissionStage.CONTRACTING_PERFORMANCE;
    }
    if (Boolean.TRUE.equals(scapSummaryView.actualTenderSummaryView().hasActualTenderActivities())) {
      return ScapSubmissionStage.ACTUAL_TENDER;
    }
    if (Boolean.TRUE.equals(scapSummaryView.plannedTenderSummaryView().hasPlannedTender())) {
      return ScapSubmissionStage.PLANNED_TENDER;
    }
    if (Objects.nonNull(scapSummaryView.projectDetailsSummaryView().projectName())) {
      return ScapSubmissionStage.CONTRACTING_STRATEGY_PENDING;
    }
    return ScapSubmissionStage.DRAFT;
  }
}
