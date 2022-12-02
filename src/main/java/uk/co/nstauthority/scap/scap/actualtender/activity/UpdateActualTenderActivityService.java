package uk.co.nstauthority.scap.scap.actualtender.activity;

import javax.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.scap.contractingperformance.ContractingPerformanceService;

@Service
class UpdateActualTenderActivityService {

  private final ActualTenderActivityService actualTenderActivityService;
  private final ContractingPerformanceService contractingPerformanceService;

  @Autowired
  UpdateActualTenderActivityService(ActualTenderActivityService actualTenderActivityService,
                                    ContractingPerformanceService contractingPerformanceService) {
    this.actualTenderActivityService = actualTenderActivityService;
    this.contractingPerformanceService = contractingPerformanceService;
  }

  @Transactional
  void updateActualTenderActivity(ActualTenderActivity actualTenderActivity, ActualTenderActivityForm form) {
    //TODO SCAP2022-41: when adding multiple companies, only delete those which have been removed from ITT,
    // and only add those that do not already exist
    if (ContractStage.CONTRACT_AWARDED.equals(actualTenderActivity.getContractStage())
        && !ContractStage.CONTRACT_AWARDED.equals(form.getContractStage())) {
      contractingPerformanceService.deleteByActualTenderActivity(actualTenderActivity);
    }
    actualTenderActivityService.saveActualTenderActivity(actualTenderActivity, form);
  }
}
