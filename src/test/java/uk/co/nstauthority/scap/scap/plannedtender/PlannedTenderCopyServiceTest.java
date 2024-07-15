package uk.co.nstauthority.scap.scap.plannedtender;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
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
import uk.co.nstauthority.scap.scap.copy.EntityCopyService;
import uk.co.nstauthority.scap.scap.detail.NewScapType;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.plannedtender.activity.PlannedTenderActivityService;

@ExtendWith(MockitoExtension.class)
class PlannedTenderCopyServiceTest {

  EntityManager entityManager = mock(EntityManager.class);
  PlannedTenderService plannedTenderService = mock(PlannedTenderService.class);
  PlannedTenderActivityService plannedTenderActivityService = mock(PlannedTenderActivityService.class);
  PlannedTenderRepository plannedTenderRepository = mock(PlannedTenderRepository.class);
  EntityCopyService entityCopyService = new EntityCopyService(entityManager);

  PlannedTenderCopyService plannedTenderCopyService = new PlannedTenderCopyService(plannedTenderRepository,
      plannedTenderService,
      plannedTenderActivityService,
      entityCopyService);

  @Captor
  ArgumentCaptor<PlannedTender> plannedTenderCaptor;

  @Test
  void copyEntity_PlannedTender() {
    var oldScapDetail = new ScapDetail();
    var newScapDetail = new ScapDetail();

    var oldPlannedTender = new PlannedTender();
    oldPlannedTender.setId(1000);
    oldPlannedTender.setHasPlannedTenders(true);
    oldPlannedTender.setHasMorePlannedTenderActivities(HasMorePlannedTenderActivities.NO);

    when(plannedTenderService.getByScapDetail(oldScapDetail))
        .thenReturn(oldPlannedTender);
    plannedTenderCopyService.copyEntity(oldScapDetail, newScapDetail, NewScapType.REINSTATEMENT);
    verify(entityManager).persist(plannedTenderCaptor.capture());
    verify(plannedTenderRepository, times(0)).save(any(PlannedTender.class));

    var result = plannedTenderCaptor.getValue();
    assertThat(result).isNotEqualTo(oldPlannedTender);
    assertValuesEqual(result, oldPlannedTender, List.of("id", "scapDetail", "createdTimestamp"));
    assertThat(result.getScapDetail()).isEqualTo(newScapDetail);
    assertThat(result.getId()).isNull();
  }

  @Test
  void copyEntity_NewDraft() {
    var oldScapDetail = new ScapDetail();
    var newScapDetail = new ScapDetail();

    var oldPlannedTender = new PlannedTender();

    when(plannedTenderService.getByScapDetail(oldScapDetail))
        .thenReturn(oldPlannedTender);
    plannedTenderCopyService.copyEntity(oldScapDetail, newScapDetail, NewScapType.DRAFT_UPDATE);
    verify(entityManager).persist(any(PlannedTender.class));
    verify(plannedTenderRepository).save(plannedTenderCaptor.capture());
    var result = plannedTenderCaptor.getValue();
    assertThat(result)
        .extracting(
            PlannedTender::getHasPlannedTenders,
            PlannedTender::getHasMorePlannedTenderActivities)
        .containsExactly(
            null,
            null
        );
  }
}
