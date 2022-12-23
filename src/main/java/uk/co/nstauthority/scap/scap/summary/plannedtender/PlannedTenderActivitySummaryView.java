package uk.co.nstauthority.scap.scap.summary.plannedtender;

import java.math.BigDecimal;
import uk.co.nstauthority.scap.scap.RemunerationModel;
import uk.co.nstauthority.scap.scap.plannedtender.activity.PlannedTenderActivity;

public record PlannedTenderActivitySummaryView(String scopeDescription,
                                               BigDecimal estimatedValue,
                                               RemunerationModel remunerationModel,
                                               String remunerationModelName,
                                               String awardRationale) {
  public static PlannedTenderActivitySummaryView from(PlannedTenderActivity plannedTenderActivity) {
    return new PlannedTenderActivitySummaryView(
        plannedTenderActivity.getScopeDescription(),
        plannedTenderActivity.getEstimatedValue(),
        plannedTenderActivity.getRemunerationModel(),
        plannedTenderActivity.getRemunerationModelName(),
        plannedTenderActivity.getAwardRationale()
    );
  }

}
