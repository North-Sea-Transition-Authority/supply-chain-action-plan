package uk.co.nstauthority.scap.scap.summary.plannedtender;

import java.math.BigDecimal;
import uk.co.nstauthority.scap.scap.RemunerationModel;
import uk.co.nstauthority.scap.scap.plannedtender.activity.PlannedTenderActivity;
import uk.co.nstauthority.scap.util.DateUtil;

public record PlannedTenderActivitySummaryView(String scopeDescription,
                                               BigDecimal estimatedValue,
                                               RemunerationModel remunerationModel,
                                               String remunerationModelName,
                                               String awardRationale,
                                               String indicativeStartDate,
                                               String indicativeAwardDate) {
  public static PlannedTenderActivitySummaryView from(PlannedTenderActivity plannedTenderActivity) {
    return new PlannedTenderActivitySummaryView(
        plannedTenderActivity.getScopeDescription(),
        plannedTenderActivity.getEstimatedValue(),
        plannedTenderActivity.getRemunerationModel(),
        plannedTenderActivity.getRemunerationModelName(),
        plannedTenderActivity.getAwardRationale(),
        DateUtil.localDateToString(plannedTenderActivity.getExpectedActualTenderStartDate()),
        DateUtil.localDateToString(plannedTenderActivity.getExpectedContractAwardDate())
    );
  }

}
