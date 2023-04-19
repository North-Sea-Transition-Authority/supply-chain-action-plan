package uk.co.nstauthority.scap.scap.plannedtender;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.junit.jupiter.api.Assertions.assertTrue;
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
import uk.co.nstauthority.scap.error.exception.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.utils.EntityTestingUtil;

@ExtendWith(MockitoExtension.class)
class PlannedTenderServiceTest {

  @Mock
  PlannedTenderRepository plannedTenderRepository;

  @Mock
  Clock clock = Clock.fixed(Instant.ofEpochSecond(1667576106), ZoneId.systemDefault());

  @InjectMocks
  PlannedTenderService plannedTenderService;

  private ScapDetail scapDetail;

  @BeforeEach
  void setup() {
    scapDetail = new ScapDetail(null, 1, true, ScapDetailStatus.DRAFT, EntityTestingUtil.dateToInstant(2000, 4, 23), 1);
  }

  @Test
  void createPlannedTenderForScapDetail_verifySaves() {
    var argumentCaptor = ArgumentCaptor.forClass(PlannedTender.class);

    plannedTenderService.createPlannedTenderForScapDetail(scapDetail);

    verify(plannedTenderRepository)
        .save(argumentCaptor.capture());

    var scapPlannedTender = argumentCaptor.getValue();
    assertThat(scapPlannedTender).extracting(
        PlannedTender::getScapDetail,
        PlannedTender::getHasPlannedTenders
    ).containsExactly(
        scapDetail,
        null
    );
  }

  @Test
  void updatePlannedTenderCompletionStatus_verifySaves() {
    var plannedTender = new PlannedTender(scapDetail, EntityTestingUtil.dateToInstant(2000, 4, 23));
    var argumentCaptor=  ArgumentCaptor.forClass(PlannedTender.class);

    plannedTenderService.updatePlannedTenderHasPlannedTenders(
        plannedTender,
        true
    );

    verify(plannedTenderRepository).save(argumentCaptor.capture());

    var savedPlannedTender = argumentCaptor.getValue();
    assertTrue(savedPlannedTender.getHasPlannedTenders());
  }

  @Test
  void getScapPlannedTenderByScapDetail_assertCorrectReturn() {
    var plannedTender = new PlannedTender(scapDetail, EntityTestingUtil.dateToInstant(2000, 4, 23));

    when(plannedTenderRepository.findByScapDetail(scapDetail)).thenReturn(Optional.of(plannedTender));

    var foundPlannedTender = plannedTenderService
        .findByScapDetail(scapDetail);

    assertThat(foundPlannedTender).isEqualTo(Optional.of(plannedTender));
  }

  @Test
  void getScapPlannedTenderByScapDetailOrThrow_assertThrows() {
    when(plannedTenderRepository.findByScapDetail(scapDetail)).thenReturn(Optional.empty());

    assertThatThrownBy(() -> plannedTenderService.getByScapDetail(scapDetail))
        .isInstanceOf(ScapEntityNotFoundException.class);
  }

  @Test
  void updatePlannedTenderHasMorePlannedTenders() {
    var plannedTender = new PlannedTender(scapDetail, EntityTestingUtil.dateToInstant(2000, 12, 30));
    plannedTender.setHasMorePlannedTenderActivities(HasMorePlannedTenderActivities.YES_LATER);
    var hasMorePlannedTenderActivities = HasMorePlannedTenderActivities.NO;
    var argumentCaptor = ArgumentCaptor.forClass(PlannedTender.class);

    plannedTenderService.updatePlannedTenderHasMorePlannedTenders(plannedTender, hasMorePlannedTenderActivities);

    verify(plannedTenderRepository).save(argumentCaptor.capture());

    assertThat(argumentCaptor.getValue().getHasMorePlannedTenderActivities()).isEqualTo(hasMorePlannedTenderActivities);
  }
}
