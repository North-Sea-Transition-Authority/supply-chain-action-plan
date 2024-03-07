package uk.co.nstauthority.scap.scap.summary.contractingperformance;

import java.math.BigDecimal;
import uk.co.nstauthority.scap.scap.RemunerationModel;

public record ContractingPerformanceSummaryDto(Integer contractingPerformanceId,
                                               String scopeTitle,
                                               String scopeDescription,
                                               BigDecimal awardValue,
                                               RemunerationModel remunerationModel,
                                               String remunerationModelName,
                                               String contractor,
                                               Integer countryId,
                                               BigDecimal outturnCost,
                                               String outturnRationale) {



}
