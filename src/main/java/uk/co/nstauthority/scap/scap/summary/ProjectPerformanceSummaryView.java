package uk.co.nstauthority.scap.scap.summary;

import java.math.BigDecimal;

public record ProjectPerformanceSummaryView(Boolean projectCompleted,
                                            String startDate,
                                            String completionDate,
                                            BigDecimal outturnCost) {
}
