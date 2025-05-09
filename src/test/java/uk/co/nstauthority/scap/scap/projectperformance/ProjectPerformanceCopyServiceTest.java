package uk.co.nstauthority.scap.scap.projectperformance;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static uk.co.nstauthority.scap.utils.ObjectTestingUtil.assertValuesEqual;

import jakarta.persistence.EntityManager;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;
import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.error.exception.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.scap.copy.EntityCopyService;
import uk.co.nstauthority.scap.scap.detail.NewScapType;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.scap.Scap;

@ExtendWith(MockitoExtension.class)
class ProjectPerformanceCopyServiceTest {


  EntityManager entityManager = mock(EntityManager.class);

  EntityCopyService entityCopyService = new EntityCopyService(entityManager);

  ProjectPerformanceRepository projectPerformanceRepository = mock(ProjectPerformanceRepository.class);

  ProjectPerformanceService projectPerformanceService = mock(ProjectPerformanceService.class);

  ProjectPerformanceCopyService projectPerformanceCopyService =
      new ProjectPerformanceCopyService(
          projectPerformanceService,
          projectPerformanceRepository,
          entityCopyService,
          entityManager);

  @Captor
  ArgumentCaptor<ProjectPerformance> performanceCaptor;

  @Test
  void copyService_copyChild_ProjectPerformance() {
    var oldScapDetail = new ScapDetail();
    var newScapDetail = new ScapDetail();

    var oldProjectPerformance = new ProjectPerformance();
    oldProjectPerformance.setProjectCompleted(true);
    oldProjectPerformance.setCompletionDate(LocalDate.now());
    oldProjectPerformance.setOutturnCost(new BigDecimal(5000));
    oldProjectPerformance.setScapDetail(oldScapDetail);
    oldProjectPerformance.setId(1111);

    when(projectPerformanceService.findByScapDetail(oldScapDetail)).thenReturn(Optional.of(oldProjectPerformance));

    projectPerformanceCopyService.copyEntity(oldScapDetail, newScapDetail, NewScapType.REINSTATEMENT);
    verify(entityManager).persist(performanceCaptor.capture());
    var result = performanceCaptor.getValue();
    assertValuesEqual(result, oldProjectPerformance, List.of("id", "scapDetail", "createdTimestamp"));
    assertThat(result.getScapDetail()).isEqualTo(newScapDetail);
    assertThat(result.getId()).isNull();
  }

  @Test
  void copyService_ProjectPerformance_NotExists() {
    var oldScapDetail = new ScapDetail();
    oldScapDetail.setScap(new Scap(5000));

    when(projectPerformanceService.findByScapDetail(oldScapDetail)).thenReturn(Optional.empty());
    assertThatThrownBy(() -> projectPerformanceCopyService.copyEntity(oldScapDetail, null, NewScapType.DRAFT_UPDATE))
        .isInstanceOf(ScapEntityNotFoundException.class);
  }

  @Test
  void copyService_copyChild_ProjectPerformance_Draft() {
    var oldScapDetail = new ScapDetail();
    var newScapDetail = new ScapDetail();

    var oldProjectPerformance = new ProjectPerformance();
    oldProjectPerformance.setProjectCompleted(false);

    when(projectPerformanceService.findByScapDetail(oldScapDetail)).thenReturn(Optional.of(oldProjectPerformance));

    projectPerformanceCopyService.copyEntity(oldScapDetail, newScapDetail, NewScapType.DRAFT_UPDATE);
    verify(entityManager).persist(performanceCaptor.capture());
    var result = performanceCaptor.getValue();

    var excludedFields = List.of("id", "scapDetail", "getProjectCompleted", "createdTimestamp", "projectCompleted");
    assertValuesEqual(result, oldProjectPerformance, excludedFields);
    assertThat(result.getScapDetail()).isEqualTo(newScapDetail);
    assertThat(result).extracting(
        ProjectPerformance::getProjectCompleted,
        ProjectPerformance::getId
    ).containsOnlyNulls();
  }

}
