package uk.co.nstauthority.scap.scap.summary;

import java.util.Objects;
import uk.co.nstauthority.scap.scap.summary.actualtender.ActualTenderSummaryView;
import uk.co.nstauthority.scap.scap.summary.contractingperformance.ContractingPerformanceOverviewSummaryView;
import uk.co.nstauthority.scap.scap.summary.plannedtender.PlannedTenderSummaryView;

public record ScapSummaryView(ProjectDetailsSummaryView projectDetailsSummaryView,
                              PlannedTenderSummaryView plannedTenderSummaryView,
                              ActualTenderSummaryView actualTenderSummaryView,
                              ContractingPerformanceOverviewSummaryView contractingPerformanceOverviewSummaryView,
                              ProjectPerformanceSummaryView projectPerformanceSummaryView) {

  private static final String NULL_ERROR_MESSAGE = "%s must not be null";

  public ScapSummaryView {
    Objects.requireNonNull(projectDetailsSummaryView, NULL_ERROR_MESSAGE.formatted("projectDetailsSummaryView"));
    Objects.requireNonNull(plannedTenderSummaryView, NULL_ERROR_MESSAGE.formatted("plannedTenderSummaryView"));
    Objects.requireNonNull(actualTenderSummaryView, NULL_ERROR_MESSAGE.formatted("actualTenderSummaryView"));
    Objects.requireNonNull(projectPerformanceSummaryView,
        NULL_ERROR_MESSAGE.formatted("projectPerformanceSummaryView"));
    Objects.requireNonNull(contractingPerformanceOverviewSummaryView,
        NULL_ERROR_MESSAGE.formatted("contractingPerformanceOverviewSummaryView"));
  }
}
