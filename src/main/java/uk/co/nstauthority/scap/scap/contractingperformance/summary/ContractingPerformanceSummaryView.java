package uk.co.nstauthority.scap.scap.contractingperformance.summary;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.math.BigDecimal;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.RemunerationModel;
import uk.co.nstauthority.scap.scap.contractingperformance.ContractingPerformanceController;
import uk.co.nstauthority.scap.scap.contractingperformance.delete.DeleteContractingPerformanceController;

public record ContractingPerformanceSummaryView(Integer scapId,
                                                Integer contractingPerformanceId,
                                                String scopeTitle,
                                                String scopeDescription,
                                                BigDecimal awardValue,
                                                RemunerationModel remunerationModel,
                                                String remunerationModelName,
                                                String contractor,
                                                Integer countryId,
                                                BigDecimal outturnCost,
                                                String outturnRationale) {

  public String getChangeLinkUrl() {
    return ReverseRouter.route(on(ContractingPerformanceController.class)
        .renderExistingContractingPerformanceForm(scapId, contractingPerformanceId));
  }

  public String getDeleteLinkUrl() {
    return ReverseRouter.route(on(DeleteContractingPerformanceController.class)
        .renderDeleteContractingPerformanceConfirmation(scapId, contractingPerformanceId));
  }

}
