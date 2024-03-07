package uk.co.nstauthority.scap.scap.summary.contractingperformance;

import java.util.List;

public record ContractingPerformanceOverviewSummaryView(
    Boolean hasContractingPerformance, List<ContractingPerformanceSummaryView> contractingPerformanceSummaryViews) {
}
