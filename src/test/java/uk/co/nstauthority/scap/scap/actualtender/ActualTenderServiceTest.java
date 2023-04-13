package uk.co.nstauthority.scap.scap.actualtender;

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
import uk.co.nstauthority.scap.error.exception.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.scap.actualtender.summary.HasMoreActualTenderActivities;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;

@ExtendWith(MockitoExtension.class)
class ActualTenderServiceTest {

  @Mock
  ActualTenderRepository actualTenderRepository;

  @Mock
  Clock clock = Clock.fixed(Instant.ofEpochSecond(1667576106), ZoneId.systemDefault());

  @InjectMocks
  ActualTenderService actualTenderService;

  private ScapDetail scapDetail;

  @BeforeEach
  void setup() {
    scapDetail = new ScapDetail();
  }

  @Test
  void getByScapDetail() {
    var actualTender = new ActualTender(scapDetail, clock.instant());

    when(actualTenderRepository.findByScapDetail(scapDetail)).thenReturn(Optional.of(actualTender));

    var returnedValue = actualTenderService.findByScapDetail(scapDetail);

    assertThat(returnedValue).contains(actualTender);
  }

  @Test
  void getByScapDetailOrThrow_isPresent_assertReturns() {
    var actualTender = new ActualTender(scapDetail, clock.instant());

    when(actualTenderRepository.findByScapDetail(scapDetail)).thenReturn(Optional.of(actualTender));

    var returnedValue = actualTenderService.getByScapDetail(scapDetail);

    assertThat(returnedValue).isEqualTo(actualTender);
  }

  @Test
  void getByScapDetailOrThrow_isNotPresent_assertThrows() {
    when(actualTenderRepository.findByScapDetail(scapDetail)).thenReturn(Optional.empty());

    assertThatThrownBy(() -> actualTenderService.getByScapDetail(scapDetail))
        .isInstanceOf(ScapEntityNotFoundException.class);
  }

  @Test
  void createActualTender() {
    var argumentCaptor = ArgumentCaptor.forClass(ActualTender.class);

    actualTenderService.createActualTender(scapDetail, YesNo.YES);

    verify(actualTenderRepository).save(argumentCaptor.capture());
    assertThat(argumentCaptor.getValue()).extracting(ActualTender::getHasActualTenders, ActualTender::getScapDetail)
        .containsExactly(true, scapDetail);
  }

  @Test
  void updateHasActualTenders() {
    var argumentCaptor = ArgumentCaptor.forClass(ActualTender.class);
    var createdTimestamp = Instant.ofEpochSecond(1666885004);
    var existingActualTender = new ActualTender(scapDetail, createdTimestamp);

    actualTenderService.updateHasActualTenders(existingActualTender, YesNo.NO);

    verify(actualTenderRepository).save(argumentCaptor.capture());
    assertThat(argumentCaptor.getValue()).extracting(
        ActualTender::getHasActualTenders,
        ActualTender::getScapDetail,
        ActualTender::getCreatedTimestamp
    ).containsExactly(
        false,
        scapDetail,
        createdTimestamp
    );
  }

  @Test
  void updateAllActualTendersAdded() {
    var existingActualTender = new ActualTender(46);
    var argumentCaptor = ArgumentCaptor.forClass(ActualTender.class);
    var hasMoreActualTenderActivities = HasMoreActualTenderActivities.NO;

    actualTenderService.updateHasMoreActualTenders(existingActualTender, hasMoreActualTenderActivities);

    verify(actualTenderRepository).save(argumentCaptor.capture());

    assertThat(argumentCaptor.getValue().getHasMoreActualTenders())
        .isEqualTo(hasMoreActualTenderActivities);
  }
}
