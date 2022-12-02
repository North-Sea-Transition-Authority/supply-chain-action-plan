package uk.co.nstauthority.scap.scap.contractingperformance;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.time.Clock;
import java.time.Instant;
import java.time.ZoneId;
import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.error.exception.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivity;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivityService;

@ExtendWith(MockitoExtension.class)
class ContractingPerformanceServiceTest {

  @Mock
  ContractingPerformanceRepository contractingPerformanceRepository;

  @Mock
  ActualTenderActivityService actualTenderActivityService;

  @Mock
  Clock clock = Clock.fixed(Instant.ofEpochSecond(1668622665), ZoneId.systemDefault());

  @InjectMocks
  ContractingPerformanceService contractingPerformanceService;

  @Test
  void getById_IsFound_VerifyCallsRepository() {
    var contractingPerformanceId = 51;
    var contractingPerformance = new ContractingPerformance(contractingPerformanceId);

    when(contractingPerformanceRepository.findById(contractingPerformanceId))
        .thenReturn(Optional.of(contractingPerformance));

    var returnedContractingPerformance = contractingPerformanceService.getById(contractingPerformanceId);

    assertThat(returnedContractingPerformance).isEqualTo(contractingPerformance);
  }

  @Test
  void getById_NotFound_AssertThrows() {
    var contractingPerformanceId = 51;

    when(contractingPerformanceRepository.findById(contractingPerformanceId))
        .thenReturn(Optional.empty());

    assertThatThrownBy(() -> contractingPerformanceService.getById(contractingPerformanceId))
        .isInstanceOf(ScapEntityNotFoundException.class);
  }

  @Test
  void hasContractingPerformance_ByContractingPerformanceOverview_AssertCallsRepository() {
    var contractingPerformanceOverview = new ContractingPerformanceOverview();
    when(contractingPerformanceRepository.existsByContractingPerformanceOverview(contractingPerformanceOverview))
        .thenReturn(true);

    assertTrue(contractingPerformanceService.hasContractingPerformance(contractingPerformanceOverview));
  }

  @Test
  void hasContractingPerformance_ByActualTenderActivity_AssertCallsRepository() {
    var actualTenderActivity = new ActualTenderActivity(168);

    when(contractingPerformanceRepository.existsByActualTenderActivity(actualTenderActivity)).thenReturn(true);

    assertTrue(contractingPerformanceService.hasContractingPerformance(actualTenderActivity));
  }

  @Test
  void saveContractingPerformance_InvalidOutturnCost_AssertThrows() {
    var contractPerformance = new ContractingPerformance(49);
    var actualTenderActivity = new ActualTenderActivity(22);
    var form = new ContractingPerformanceForm();
    form.setActualTenderActivityId(actualTenderActivity.getId());
    form.setOutturnCost("NaN");
    form.setOutturnRationale("test outturn rationale");

    when(actualTenderActivityService.getById(actualTenderActivity.getId())).thenReturn(actualTenderActivity);

    assertThatIllegalArgumentException()
        .isThrownBy(() -> contractingPerformanceService.saveContractingPerformance(contractPerformance, form));

    verify(contractingPerformanceRepository, never()).save(any());
  }

  @Test
  void saveContractingPerformance_VerifyUpdates() {
    var contractPerformance = new ContractingPerformance(49);
    var actualTenderActivity = new ActualTenderActivity(22);
    var form = new ContractingPerformanceForm();
    form.setActualTenderActivityId(actualTenderActivity.getId());
    form.setOutturnCost("1.23");
    form.setOutturnRationale("test outturn rationale");
    var argumentCaptor = ArgumentCaptor.forClass(ContractingPerformance.class);

    when(actualTenderActivityService.getById(actualTenderActivity.getId())).thenReturn(actualTenderActivity);

    contractingPerformanceService.saveContractingPerformance(contractPerformance, form);

    verify(contractingPerformanceRepository).save(argumentCaptor.capture());

    assertThat(argumentCaptor.getValue()).extracting(
        ContractingPerformance::getId,
        ContractingPerformance::getActualTenderActivity,
        ContractingPerformance::getOutturnCost,
        ContractingPerformance::getOutturnRationale
    ).containsExactly(
        contractPerformance.getId(),
        actualTenderActivity,
        form.getOutturnCost().getAsBigDecimal().get(),
        form.getOutturnRationale().getInputValue()
    );
  }

  @Test
  void createContractingPerformance_VerifySaves() {
    var contractingPerformanceOverview = new ContractingPerformanceOverview();
    var actualTenderActivity = new ActualTenderActivity(22);
    var form = new ContractingPerformanceForm();
    form.setActualTenderActivityId(actualTenderActivity.getId());
    form.setOutturnCost("1.23");
    form.setOutturnRationale("test outturn rationale");
    var argumentCaptor = ArgumentCaptor.forClass(ContractingPerformance.class);

    when(actualTenderActivityService.getById(actualTenderActivity.getId())).thenReturn(actualTenderActivity);

    contractingPerformanceService.createContractingPerformance(contractingPerformanceOverview, form);

    verify(contractingPerformanceRepository).save(argumentCaptor.capture());

    assertThat(argumentCaptor.getValue()).extracting(
        ContractingPerformance::getCreatedTimestamp,
        ContractingPerformance::getContractingPerformanceOverview,
        ContractingPerformance::getActualTenderActivity,
        ContractingPerformance::getOutturnCost,
        ContractingPerformance::getOutturnRationale
    ).containsExactly(
        clock.instant(),
        contractingPerformanceOverview,
        actualTenderActivity,
        form.getOutturnCost().getAsBigDecimal().get(),
        form.getOutturnRationale().getInputValue()
    );
  }

  @Test
  void deleteContractingPerformance_VerifyDeletes() {
    var contractingPerformance = new ContractingPerformance();

    contractingPerformanceService.deleteContractingPerformance(contractingPerformance);

    verify(contractingPerformanceRepository).delete(contractingPerformance);
  }

  @Test
  void deleteByContractingPerformance_VerifyDeletes() {
    var actualTenderActivity = new ActualTenderActivity();

    contractingPerformanceService.deleteByActualTenderActivity(actualTenderActivity);

    verify(contractingPerformanceRepository).deleteByActualTenderActivity(actualTenderActivity);
  }
}
