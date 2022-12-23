package uk.co.nstauthority.scap.scap.summary.actualtender;

import java.util.List;

public record ActualTenderSummaryView(
    Boolean hasActualTenderActivities,
    List<ActualTenderActivitySummaryView> actualTenderActivitySummaryViews) {
}
