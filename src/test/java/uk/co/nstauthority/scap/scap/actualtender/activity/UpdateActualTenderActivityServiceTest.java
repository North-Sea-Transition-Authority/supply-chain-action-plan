package uk.co.nstauthority.scap.scap.actualtender.activity;

import static org.mockito.Mockito.never;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.scap.RemunerationModel;
import uk.co.nstauthority.scap.scap.contractingperformance.ContractingPerformanceService;

@ExtendWith(MockitoExtension.class)
class UpdateActualTenderActivityServiceTest {

  @Mock
  ActualTenderActivityService actualTenderActivityService;

  @Mock
  ContractingPerformanceService contractingPerformanceService;

  @InjectMocks
  UpdateActualTenderActivityService updateActualTenderActivityService;

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
    var form = getFilledActualTenderActivityForm(ContractStage.REQUEST_FOR_INFORMATION);

    var actualTenderActivity = new ActualTenderActivity();
    actualTenderActivity.setContractStage(ContractStage.CONTRACT_AWARDED);

    updateActualTenderActivityService.updateActualTenderActivity(actualTenderActivity, form);

    var inOrder = Mockito.inOrder(contractingPerformanceService, actualTenderActivityService);

    inOrder.verify(contractingPerformanceService).deleteByActualTenderActivity(actualTenderActivity);
    inOrder.verify(actualTenderActivityService).saveActualTenderActivity(actualTenderActivity, form);
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
    form.setInvitationToTenderParticipants("test ITT participant");
    return form;
  }
}
