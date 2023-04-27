package uk.co.nstauthority.scap.scap.actualtender.activity.delete;

import javax.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivity;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivityService;
import uk.co.nstauthority.scap.scap.actualtender.activity.InvitationToTenderParticipantService;
import uk.co.nstauthority.scap.scap.actualtender.activity.awardedcontract.AwardedContractService;
import uk.co.nstauthority.scap.scap.contractingperformance.ContractingPerformanceService;

@Service
class DeleteActualTenderActivityService {

  private final ActualTenderActivityService actualTenderActivityService;
  private final InvitationToTenderParticipantService invitationToTenderParticipantService;
  private final AwardedContractService awardedContractService;
  private final ContractingPerformanceService contractingPerformanceService;

  @Autowired
  DeleteActualTenderActivityService(ActualTenderActivityService actualTenderActivityService,
                                    InvitationToTenderParticipantService invitationToTenderParticipantService,
                                    AwardedContractService awardedContractService,
                                    ContractingPerformanceService contractingPerformanceService) {
    this.actualTenderActivityService = actualTenderActivityService;
    this.invitationToTenderParticipantService = invitationToTenderParticipantService;
    this.awardedContractService = awardedContractService;
    this.contractingPerformanceService = contractingPerformanceService;
  }

  @Transactional
  public void deleteActualTenderActivity(ActualTenderActivity actualTenderActivity) {
    contractingPerformanceService.deleteByActualTenderActivity(actualTenderActivity);
    awardedContractService.deleteByActualTenderActivity(actualTenderActivity);
    invitationToTenderParticipantService.deleteAllByActualTenderActivity(actualTenderActivity);
    actualTenderActivityService.deleteActualTenderActivity(actualTenderActivity);
  }
}
