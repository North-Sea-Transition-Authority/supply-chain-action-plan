package uk.co.nstauthority.scap.scap.summary;

import java.util.List;

public record PlannedTenderSummaryView(Boolean hasPlannedTender,
                                       List<PlannedTenderActivitySummaryView> plannedTenderActivitySummaryViews) {
}
