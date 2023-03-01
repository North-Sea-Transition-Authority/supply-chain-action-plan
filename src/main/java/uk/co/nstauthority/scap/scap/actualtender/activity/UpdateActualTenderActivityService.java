package uk.co.nstauthority.scap.scap.actualtender.activity;

import java.util.Collections;
import javax.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.scap.actualtender.activity.awardedcontract.AwardedContractService;
import uk.co.nstauthority.scap.scap.contractingperformance.ContractingPerformanceService;

@Service
class UpdateActualTenderActivityService {

  private final ActualTenderActivityService actualTenderActivityService;
  private final ContractingPerformanceService contractingPerformanceService;
  private final InvitationToTenderParticipantService invitationToTenderParticipantService;
  private final AwardedContractService awardedContractService;

  @Autowired
  UpdateActualTenderActivityService(ActualTenderActivityService actualTenderActivityService,
                                    ContractingPerformanceService contractingPerformanceService,
                                    InvitationToTenderParticipantService invitationToTenderParticipantService,
                                    AwardedContractService awardedContractService) {
    this.actualTenderActivityService = actualTenderActivityService;
    this.contractingPerformanceService = contractingPerformanceService;
    this.invitationToTenderParticipantService = invitationToTenderParticipantService;
    this.awardedContractService = awardedContractService;
  }

  @Transactional
  void updateActualTenderActivity(ActualTenderActivity actualTenderActivity, ActualTenderActivityForm form) {
    if (ContractStage.CONTRACT_AWARDED.equals(actualTenderActivity.getContractStage())
        && !ContractStage.CONTRACT_AWARDED.equals(form.getContractStage())) {
      contractingPerformanceService.deleteByActualTenderActivity(actualTenderActivity);
      awardedContractService.deleteByActualTenderActivity(actualTenderActivity);
    }

    if (ContractStage.INVITATION_TO_TENDER_IS_LIVE.equals(form.getContractStage())
        && !ContractStage.INVITATION_TO_TENDER_IS_LIVE.equals(actualTenderActivity.getContractStage())) {
      var ittParticipants = invitationToTenderParticipantService.getInvitationToTenderParticipants(actualTenderActivity);
      invitationToTenderParticipantService.updateBidParticipants(ittParticipants, Collections.emptyList());
    }
    actualTenderActivityService.saveActualTenderActivity(actualTenderActivity, form);
  }
}
