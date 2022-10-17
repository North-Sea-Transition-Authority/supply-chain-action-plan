package uk.co.nstauthority.scap.application.plannedtender;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Optional;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.application.detail.ScapDetail;
import uk.co.nstauthority.scap.application.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.error.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.utils.EntityTestingUtil;

@ExtendWith(MockitoExtension.class)
public class ScapPlannedTenderServiceTest {

  @Mock
  ScapPlannedTenderRepository scapPlannedTenderRepository;

  @InjectMocks
  ScapPlannedTenderService scapPlannedTenderService;

  private ScapDetail scapDetail;

  @BeforeEach
  private void setup() {
    scapDetail = new ScapDetail(null, 1, true, ScapDetailStatus.DRAFT, EntityTestingUtil.dateToInstant(2000, 4, 23), 1);
  }

  @Test
  public void createPlannedTenderForScapDetail_verifySaves() {
    var argumentCaptor = ArgumentCaptor.forClass(ScapPlannedTender.class);

    scapPlannedTenderService.createPlannedTenderForScapDetail(scapDetail);

    verify(scapPlannedTenderRepository, times(1))
        .save(argumentCaptor.capture());

    var scapPlannedTender = argumentCaptor.getValue();
    assertThat(scapPlannedTender).extracting(
        ScapPlannedTender::getScapDetail,
        ScapPlannedTender::getHasPlannedTenders
    ).containsExactly(
        scapDetail,
        null
    );
  }

  @Test
  public void updatePlannedTenderCompletionStatus_verifySaves() {
    var plannedTender = new ScapPlannedTender(scapDetail, EntityTestingUtil.dateToInstant(2000, 4, 23));
    var argumentCaptor=  ArgumentCaptor.forClass(ScapPlannedTender.class);

    scapPlannedTenderService.updatePlannedTenderHasPlannedTenders(
        plannedTender,
        true
    );

    verify(scapPlannedTenderRepository, times(1)).save(argumentCaptor.capture());

    var savedPlannedTender = argumentCaptor.getValue();
    assertThat(savedPlannedTender.getHasPlannedTenders()).isEqualTo(true);
  }

  @Test
  public void getScapPlannedTenderByScapDetail_assertCorrectReturn() {
    var plannedTender = new ScapPlannedTender(scapDetail, EntityTestingUtil.dateToInstant(2000, 4, 23));

    when(scapPlannedTenderRepository.findByScapDetail(scapDetail)).thenReturn(Optional.of(plannedTender));

    var foundPlannedTender = scapPlannedTenderService
        .getScapPlannedTenderByScapDetail(scapDetail);

    assertThat(foundPlannedTender).isEqualTo(Optional.of(plannedTender));
  }

  @Test
  public void getScapPlannedTenderByScapDetailOrThrow_assertThrows() {
    when(scapPlannedTenderRepository.findByScapDetail(scapDetail)).thenReturn(Optional.empty());

    assertThatThrownBy(() -> scapPlannedTenderService.getScapPlannedTenderByScapDetailOrThrow(scapDetail))
        .isInstanceOf(ScapEntityNotFoundException.class);
  }
}
