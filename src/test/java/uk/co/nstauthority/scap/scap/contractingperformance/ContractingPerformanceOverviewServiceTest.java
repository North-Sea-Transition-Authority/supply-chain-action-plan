package uk.co.nstauthority.scap.scap.contractingperformance;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.time.Clock;
import java.time.Instant;
import java.time.ZoneId;
import java.util.Optional;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.enumutil.YesNo;
import uk.co.nstauthority.scap.error.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;

@ExtendWith(MockitoExtension.class)
class ContractingPerformanceOverviewServiceTest {

  @Mock
  ContractingPerformanceOverviewRepository contractingPerformanceOverviewRepository;

  @Mock
  Clock clock = Clock.fixed(Instant.now(), ZoneId.systemDefault());

  @InjectMocks
  ContractingPerformanceOverviewService contractingPerformanceOverviewService;

  ScapDetail scapDetail;

  @BeforeEach
  void setup() {
    scapDetail = new ScapDetail();
  }

  @Test
  void getByScapDetail() {
    var contractingPerformanceOverview = new ContractingPerformanceOverview();

    when(contractingPerformanceOverviewRepository.findByScapDetail(scapDetail))
        .thenReturn(Optional.of(contractingPerformanceOverview));

    var returnedContractingPerformanceOverview = contractingPerformanceOverviewService
        .getByScapDetail(scapDetail);

    assertThat(returnedContractingPerformanceOverview).contains(contractingPerformanceOverview);
  }

  @Test
  void getByScapDetailOrThrow_IsFound_AssertNotThrows() {
    var contractingPerformanceOverview = new ContractingPerformanceOverview();

    when(contractingPerformanceOverviewRepository.findByScapDetail(scapDetail))
        .thenReturn(Optional.of(contractingPerformanceOverview));

    var returnedContractingPerformanceOverview = contractingPerformanceOverviewService
        .getByScapDetailOrThrow(scapDetail);

    assertThat(returnedContractingPerformanceOverview).isEqualTo(contractingPerformanceOverview);
  }

  @Test
  void getByScapDetailOrThrow_NotFound_AssertThrows() {
    when(contractingPerformanceOverviewRepository.findByScapDetail(scapDetail))
        .thenReturn(Optional.empty());

    assertThatThrownBy(() -> contractingPerformanceOverviewService.getByScapDetailOrThrow(scapDetail))
        .isInstanceOf(ScapEntityNotFoundException.class);
  }

  @Test
  void saveContractingPerformance_NewEntity() {
    var hasContractingPerformance = YesNo.YES;
    var argumentCaptor = ArgumentCaptor.forClass(ContractingPerformanceOverview.class);

    contractingPerformanceOverviewService.saveContractingPerformance(scapDetail, hasContractingPerformance);

    verify(contractingPerformanceOverviewRepository).save(argumentCaptor.capture());
    assertThat(argumentCaptor.getValue()).extracting(
        ContractingPerformanceOverview::getScapDetail,
        ContractingPerformanceOverview::getHasContractingPerformance,
        ContractingPerformanceOverview::getCreatedTimestamp
    ).containsExactly(
        scapDetail,
        Boolean.TRUE,
        clock.instant()
    );
  }

  @Test
  void saveContractingPerformance_ExistingEntity() {
    var hasContractingPerformance = YesNo.YES;
    var contractingPerformanceOverviewId = 49;
    var argumentCaptor = ArgumentCaptor.forClass(ContractingPerformanceOverview.class);
    var existingContractingPerformanceOverview = new ContractingPerformanceOverview(contractingPerformanceOverviewId);

    when(contractingPerformanceOverviewRepository.findByScapDetail(scapDetail))
        .thenReturn(Optional.of(existingContractingPerformanceOverview));

    contractingPerformanceOverviewService.saveContractingPerformance(scapDetail, hasContractingPerformance);

    verify(contractingPerformanceOverviewRepository).save(argumentCaptor.capture());
    assertThat(argumentCaptor.getValue()).extracting(
        ContractingPerformanceOverview::getId,
        ContractingPerformanceOverview::getHasContractingPerformance
    ).containsExactly(
        contractingPerformanceOverviewId,
        Boolean.TRUE
    );
  }

}
