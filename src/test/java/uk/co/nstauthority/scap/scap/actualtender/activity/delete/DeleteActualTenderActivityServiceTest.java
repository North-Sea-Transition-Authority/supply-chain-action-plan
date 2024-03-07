package uk.co.nstauthority.scap.scap.actualtender.activity.delete;

import static org.mockito.Mockito.inOrder;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.scap.actualtender.ActualTenderService;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivity;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivityService;
import uk.co.nstauthority.scap.scap.actualtender.activity.InvitationToTenderParticipantService;
import uk.co.nstauthority.scap.scap.actualtender.activity.awardedcontract.AwardedContractService;
import uk.co.nstauthority.scap.scap.contractingperformance.ContractingPerformanceOverviewService;
import uk.co.nstauthority.scap.scap.contractingperformance.ContractingPerformanceService;

@ExtendWith(MockitoExtension.class)
class DeleteActualTenderActivityServiceTest {

  @Mock
  ActualTenderActivityService actualTenderActivityService;

  @Mock
  InvitationToTenderParticipantService invitationToTenderParticipantService;

  @Mock
  AwardedContractService awardedContractService;

  @Mock
  ContractingPerformanceService contractingPerformanceService;

  @Mock
  ActualTenderService actualTenderService;

  @Mock
  ContractingPerformanceOverviewService contractingPerformanceOverviewService;

  @InjectMocks
  DeleteActualTenderActivityService deleteActualTenderActivityService;

  @Test
  void deleteActualTenderActivity_VerifyCalls() {
    var actualTenderActivity = new ActualTenderActivity(44);

    deleteActualTenderActivityService.deleteActualTenderActivity(actualTenderActivity);

    var inOrder = inOrder(
        contractingPerformanceService,
        awardedContractService,
        invitationToTenderParticipantService,
        actualTenderActivityService
    );

    inOrder.verify(contractingPerformanceService).deleteByActualTenderActivity(actualTenderActivity);
    inOrder.verify(awardedContractService).deleteByActualTenderActivity(actualTenderActivity);
    inOrder.verify(invitationToTenderParticipantService).deleteAllByActualTenderActivity(actualTenderActivity);
    inOrder.verify(actualTenderActivityService).deleteActualTenderActivity(actualTenderActivity);
  }
}
