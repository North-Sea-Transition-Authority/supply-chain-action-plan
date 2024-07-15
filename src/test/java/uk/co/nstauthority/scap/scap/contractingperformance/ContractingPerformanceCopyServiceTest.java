package uk.co.nstauthority.scap.scap.contractingperformance;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static uk.co.nstauthority.scap.utils.ObjectTestingUtil.assertValuesEqual;

import jakarta.persistence.EntityManager;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.scap.contractingperformance.summary.HasMoreContractingPerformance;
import uk.co.nstauthority.scap.scap.copy.EntityCopyService;
import uk.co.nstauthority.scap.scap.detail.NewScapType;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;

@ExtendWith(MockitoExtension.class)
class ContractingPerformanceCopyServiceTest {


  EntityManager entityManager = mock(EntityManager.class);

  EntityCopyService entityCopyService = new EntityCopyService(entityManager);

  ContractingPerformanceOverviewService contractingPerformanceOverviewService = mock(ContractingPerformanceOverviewService.class);

  ContractingPerformanceOverviewRepository contractingPerformanceOverviewRepository = mock(ContractingPerformanceOverviewRepository.class);

  ContractingPerformanceCopyService contractingPerformanceCopyService =
      new ContractingPerformanceCopyService(contractingPerformanceOverviewService,
          contractingPerformanceOverviewRepository,
          entityCopyService
      );

  @Captor
  ArgumentCaptor<ContractingPerformanceOverview> contractingCaptor;

  @Test
  void copyService_copyChild_ContractingPerformance() {
    var oldScapDetail = new ScapDetail();
    var newScapDetail = new ScapDetail();

    var oldContractingPerformanceOverview = new ContractingPerformanceOverview();
    oldContractingPerformanceOverview.setHasMoreContractingPerformance(HasMoreContractingPerformance.NO);
    oldContractingPerformanceOverview.setHasContractingPerformance(true);
    oldContractingPerformanceOverview.setScapDetail(oldScapDetail);
    oldContractingPerformanceOverview.setId(5000);

    when(contractingPerformanceOverviewService.getByScapDetail(oldScapDetail)).thenReturn(oldContractingPerformanceOverview);

    contractingPerformanceCopyService.copyEntity(oldScapDetail, newScapDetail, NewScapType.REINSTATEMENT);
    verify(entityManager).persist(contractingCaptor.capture());
    var result = contractingCaptor.getValue();
    assertThat(result.getId()).isNotEqualTo(oldContractingPerformanceOverview.getId());
    assertValuesEqual(result, oldContractingPerformanceOverview, List.of("id", "scapDetail", "createdTimestamp"));
    assertThat(result.getScapDetail()).isEqualTo(newScapDetail);
    assertThat(result.getId()).isNull();
  }

  @Test
  void copyService_copyChild_ContractingPerformance_isNewUpdate() {
    var oldScapDetail = new ScapDetail();
    var newScapDetail = new ScapDetail();

    var oldContractingPerformanceOverview = new ContractingPerformanceOverview();
    oldContractingPerformanceOverview.setHasMoreContractingPerformance(HasMoreContractingPerformance.NO);
    oldContractingPerformanceOverview.setHasContractingPerformance(true);
    oldContractingPerformanceOverview.setScapDetail(oldScapDetail);
    oldContractingPerformanceOverview.setId(5000);

    when(contractingPerformanceOverviewService.getByScapDetail(oldScapDetail)).thenReturn(oldContractingPerformanceOverview);

    contractingPerformanceCopyService.copyEntity(oldScapDetail, newScapDetail, NewScapType.DRAFT_UPDATE);
    verify(contractingPerformanceOverviewRepository).save(contractingCaptor.capture());
    var result = contractingCaptor.getValue();
    assertThat(result)
        .extracting(
            ContractingPerformanceOverview::getHasContractingPerformance,
            ContractingPerformanceOverview::getHasMoreContractingPerformance,
            ContractingPerformanceOverview::getScapDetail,
            ContractingPerformanceOverview::getId
        )
        .containsExactly(
            null,
            null,
            newScapDetail,
            null
        );
  }
}
