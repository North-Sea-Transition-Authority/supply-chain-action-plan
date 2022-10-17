package uk.co.nstauthority.scap.application.detail;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.application.overview.ScapOverview;
import uk.co.nstauthority.scap.error.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.utils.EntityTestingUtil;

@ExtendWith(MockitoExtension.class)
public class ScapDetailServiceTest {

  @Mock
  ScapDetailRepository scapDetailRepository;

  @InjectMocks
  ScapDetailService scapDetailService;

  private ScapOverview scap;

  @BeforeEach
  public void setup() {
    scap = new ScapOverview(1664);
  }

  @Test
  public void createDraftScapDetail_verifySaves() {
    scapDetailService.createDraftScapDetail(scap);

    var argumentCaptor = ArgumentCaptor.forClass(ScapDetail.class);

    verify(scapDetailRepository, times(1)).save(argumentCaptor.capture());

    var scapDetail = argumentCaptor.getValue();

    assertTrue(scapDetail.getTipFlag());
    assertThat(scapDetail.getScap()).isEqualTo(scap);
    assertThat(scapDetail.getStatus()).isEqualTo(ScapDetailStatus.DRAFT);
    assertThat(scapDetail.getVersionNumber()).isEqualTo(1);
  }

  @Test
  public void getLatestScapDetailByScap_verifyCalls() {
    var scapDetailList = List.of(
        new ScapDetail(scap, 1, false, ScapDetailStatus.DRAFT, EntityTestingUtil.dateToInstant(2000, 4, 23), 1),
        new ScapDetail(scap, 2, true, ScapDetailStatus.DRAFT, EntityTestingUtil.dateToInstant(2000, 4, 23), 1)
    );
    when(scapDetailRepository.findAllByScap(scap)).thenReturn(scapDetailList);

    var scapDetail = scapDetailService.getLatestScapDetailByScap(scap);

    assertThat(scapDetail.get()).isEqualTo(scapDetailList.get(1));
  }

  @Test
  public void getLatestScapDetailByScapOrThrow_assertThrows() {
    when(scapDetailRepository.findAllByScap(scap)).thenReturn(Collections.emptyList());

    assertThatThrownBy(() -> scapDetailService.getLatestScapDetailByScapOrThrow(scap))
        .isInstanceOf(ScapEntityNotFoundException.class);
  }
}
