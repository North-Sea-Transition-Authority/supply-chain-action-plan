package uk.co.nstauthority.scap.scap.actualtender.activity;

import static org.mockito.Mockito.never;

import java.util.Collections;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.scap.RemunerationModel;
import uk.co.nstauthority.scap.scap.actualtender.activity.awardedcontract.AwardedContractService;
import uk.co.nstauthority.scap.scap.contractingperformance.ContractingPerformanceService;

@ExtendWith(MockitoExtension.class)
class UpdateActualTenderActivityServiceTest {

  @Mock
  ActualTenderActivityService actualTenderActivityService;

  @Mock
  ContractingPerformanceService contractingPerformanceService;

  @Mock
  AwardedContractService awardedContractService;

  @Mock
  InvitationToTenderParticipantService invitationToTenderParticipantService;

  @InjectMocks
  UpdateActualTenderActivityService updateActualTenderActivityService;

  @Mock
  List<InvitationToTenderParticipant> ittParticipants;

  @Test
  void updateActualTenderActivity_AssertSaves() {
    var form = getFilledActualTenderActivityForm(ContractStage.CONTRACT_AWARDED);

    var actualTenderActivity = new ActualTenderActivity();

    updateActualTenderActivityService.updateActualTenderActivity(actualTenderActivity, form);

    var inOrder = Mockito.inOrder(contractingPerformanceService, actualTenderActivityService);

    inOrder.verify(contractingPerformanceService, never()).deleteByActualTenderActivity(actualTenderActivity);
    inOrder.verify(actualTenderActivityService).saveActualTenderActivity(actualTenderActivity, form);
  }

  @Test
  void updateActualTenderActivity_RemovingAwardedContract_AssertSaveAndDelete() {
    var form = getFilledActualTenderActivityForm(ContractStage.INVITATION_TO_TENDER_IS_LIVE);

    var actualTenderActivity = new ActualTenderActivity();
    actualTenderActivity.setContractStage(ContractStage.CONTRACT_AWARDED);

    updateActualTenderActivityService.updateActualTenderActivity(actualTenderActivity, form);

    var inOrder = Mockito.inOrder(contractingPerformanceService, actualTenderActivityService);

    inOrder.verify(contractingPerformanceService).deleteByActualTenderActivity(actualTenderActivity);
    inOrder.verify(actualTenderActivityService).saveActualTenderActivity(actualTenderActivity, form);
  }

  @Test
  void updateActualTenderActivity_AtItt_AssertDoesNotUpdateBidParticipants() {
    var form = getFilledActualTenderActivityForm(ContractStage.INVITATION_TO_TENDER_IS_LIVE);
    var actualTenderActivity = new ActualTenderActivity();
    actualTenderActivity.setContractStage(ContractStage.INVITATION_TO_TENDER_IS_LIVE);

    updateActualTenderActivityService.updateActualTenderActivity(actualTenderActivity, form);

    var inOrder = Mockito.inOrder(
        contractingPerformanceService,
        actualTenderActivityService,
        awardedContractService,
        invitationToTenderParticipantService
    );

    inOrder.verify(actualTenderActivityService).saveActualTenderActivity(actualTenderActivity, form);
    inOrder.verifyNoMoreInteractions();
  }

  @Test
  void updateActualTenderActivity_ContractStageUnchanged_AssertNeverDeletes() {
    var form = getFilledActualTenderActivityForm(ContractStage.CONTRACT_AWARDED);

    var actualTenderActivity = new ActualTenderActivity();
    actualTenderActivity.setContractStage(ContractStage.CONTRACT_AWARDED);

    updateActualTenderActivityService.updateActualTenderActivity(actualTenderActivity, form);

    var inOrder = Mockito.inOrder(contractingPerformanceService, actualTenderActivityService);

    inOrder.verify(contractingPerformanceService, never()).deleteByActualTenderActivity(actualTenderActivity);
    inOrder.verify(actualTenderActivityService).saveActualTenderActivity(actualTenderActivity, form);
  }

  private ActualTenderActivityForm getFilledActualTenderActivityForm(ContractStage contractStage) {
    var form = new ActualTenderActivityForm();
    form.setScopeTitle("test scope title");
    form.setScopeDescription("test scope description");
    form.setRemunerationModel(RemunerationModel.OTHER);
    form.setRemunerationModelName("test remuneration model name");
    form.setContractStage(contractStage);
    form.setInvitationToTenderParticipants(Collections.singletonList("test ITT participant"));
    return form;
  }
}
