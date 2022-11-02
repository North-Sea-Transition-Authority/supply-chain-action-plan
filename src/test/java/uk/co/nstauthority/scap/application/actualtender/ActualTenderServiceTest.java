package uk.co.nstauthority.scap.application.actualtender;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.time.Instant;
import java.util.Optional;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.application.detail.ScapDetail;
import uk.co.nstauthority.scap.enumutil.YesNo;

@ExtendWith(MockitoExtension.class)
class ActualTenderServiceTest {

  @Mock
  ActualTenderRepository actualTenderRepository;

  @InjectMocks
  ActualTenderService actualTenderService;

  private ScapDetail scapDetail;

  @BeforeEach
  void setup() {
    scapDetail = new ScapDetail();
  }

  @Test
  void getByScapDetail() {
    var actualTender = new ActualTender(scapDetail, Instant.now());

    when(actualTenderRepository.findByScapDetail(scapDetail)).thenReturn(Optional.of(actualTender));

    var returnedValue = actualTenderService.getByScapDetail(scapDetail);

    assertThat(returnedValue).contains(actualTender);
  }

  @Test
  void createActualTender() {
    var argumentCaptor = ArgumentCaptor.forClass(ActualTender.class);

    actualTenderService.createActualTender(scapDetail, YesNo.YES);

    verify(actualTenderRepository, times(1)).save(argumentCaptor.capture());
    assertThat(argumentCaptor.getValue()).extracting(ActualTender::getHasActualTenders, ActualTender::getScapDetail)
        .containsExactly(true, scapDetail);
  }

  @Test
  void updateHasActualTenders() {
    var argumentCaptor = ArgumentCaptor.forClass(ActualTender.class);
    var createdTimestamp = Instant.ofEpochSecond(1666885004);
    var existingActualTender = new ActualTender(scapDetail, createdTimestamp);

    actualTenderService.updateHasActualTenders(existingActualTender, YesNo.NO);

    verify(actualTenderRepository, times(1)).save(argumentCaptor.capture());
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
}
