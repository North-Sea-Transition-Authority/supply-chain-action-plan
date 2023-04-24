package uk.co.nstauthority.scap.scap.summary;

import java.util.Collections;
import uk.co.nstauthority.scap.scap.summary.actualtender.ActualTenderSummaryView;
import uk.co.nstauthority.scap.scap.summary.contractingperformance.ContractingPerformanceOverviewSummaryView;
import uk.co.nstauthority.scap.scap.summary.plannedtender.PlannedTenderSummaryView;

public class ScapSummaryControllerTestUtil {

  private static final ProjectDetailsSummaryView PROJECT_DETAILS_SUMMARY_VIEW = new ProjectDetailsSummaryView(
      null, null, Collections.emptyList(), null, null, null,
      null, null, Collections.emptyList(), null, null, Collections.emptyList()
  );
  private static final PlannedTenderSummaryView PLANNED_TENDER_SUMMARY_VIEW = new PlannedTenderSummaryView(
      null,
      null
  );
  private static final ActualTenderSummaryView ACTUAL_TENDER_SUMMARY_VIEW = new ActualTenderSummaryView(
      null, null
  );
  private static final ContractingPerformanceOverviewSummaryView CONTRACTING_PERFORMANCE_OVERVIEW_SUMMARY_VIEW =
      new ContractingPerformanceOverviewSummaryView(null, null);
  private static final ProjectPerformanceSummaryView PROJECT_PERFORMANCE_SUMMARY_VIEW =
      new ProjectPerformanceSummaryView(null, null, null, null);

  private static final ScapSummaryView SCAP_SUMMARY_VIEW = new ScapSummaryView(
      PROJECT_DETAILS_SUMMARY_VIEW,
      PLANNED_TENDER_SUMMARY_VIEW,
      ACTUAL_TENDER_SUMMARY_VIEW,
      CONTRACTING_PERFORMANCE_OVERVIEW_SUMMARY_VIEW,
      PROJECT_PERFORMANCE_SUMMARY_VIEW
  );

  public static ProjectDetailsSummaryView getProjectDetailsSummaryView() {
    return PROJECT_DETAILS_SUMMARY_VIEW;
  }

  public static PlannedTenderSummaryView getPlannedTenderSummaryView() {
    return PLANNED_TENDER_SUMMARY_VIEW;
  }

  public static ActualTenderSummaryView getActualTenderSummaryView() {
    return ACTUAL_TENDER_SUMMARY_VIEW;
  }

  public static ContractingPerformanceOverviewSummaryView getContractingPerformanceOverviewSummaryView() {
    return CONTRACTING_PERFORMANCE_OVERVIEW_SUMMARY_VIEW;
  }

  public static ProjectPerformanceSummaryView getProjectPerformanceSummaryView() {
    return PROJECT_PERFORMANCE_SUMMARY_VIEW;
  }

  public static ScapSummaryView getScapSummaryView() {
    return SCAP_SUMMARY_VIEW;
  }
}
