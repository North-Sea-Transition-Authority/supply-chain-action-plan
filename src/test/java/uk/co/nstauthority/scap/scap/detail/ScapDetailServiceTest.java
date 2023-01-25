package uk.co.nstauthority.scap.scap.detail;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.time.Clock;
import java.time.Instant;
import java.time.ZoneId;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.authentication.ServiceUserDetail;
import uk.co.nstauthority.scap.authentication.UserDetailService;
import uk.co.nstauthority.scap.error.exception.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.scap.scap.Scap;
import uk.co.nstauthority.scap.scap.submit.ReviewAndSubmitForm;
import uk.co.nstauthority.scap.utils.EntityTestingUtil;

@ExtendWith(MockitoExtension.class)
class ScapDetailServiceTest {

  @Mock
  ScapDetailRepository scapDetailRepository;

  @Mock
  UserDetailService userDetailService;

  @Mock
  Clock clock = Clock.fixed(Instant.ofEpochSecond(1667576106), ZoneId.systemDefault());

  @InjectMocks
  ScapDetailService scapDetailService;

  private Scap scap;

  @BeforeEach
  void setup() {
    scap = new Scap(1664);
  }

  @Test
  void createDraftScapDetail_verifySaves() {
    when(userDetailService.getUserDetail()).thenReturn(getUserDetail());
    scapDetailService.createDraftScapDetail(scap);

    var argumentCaptor = ArgumentCaptor.forClass(ScapDetail.class);

    verify(scapDetailRepository).save(argumentCaptor.capture());

    var scapDetail = argumentCaptor.getValue();

    assertTrue(scapDetail.getTipFlag());
    assertThat(scapDetail.getScap()).isEqualTo(scap);
    assertThat(scapDetail.getStatus()).isEqualTo(ScapDetailStatus.DRAFT);
    assertThat(scapDetail.getVersionNumber()).isEqualTo(1);
    assertThat(scapDetail.getCreatedByUserId()).isEqualTo(1000);
  }

  @Test
  void getLatestScapDetailByScap_verifyCalls() {
    var scapDetailList = List.of(
        new ScapDetail(scap, 1, false, ScapDetailStatus.DRAFT, EntityTestingUtil.dateToInstant(2000, 4, 23), 1),
        new ScapDetail(scap, 2, true, ScapDetailStatus.DRAFT, EntityTestingUtil.dateToInstant(2000, 4, 23), 1)
    );
    when(scapDetailRepository.findAllByScap(scap)).thenReturn(scapDetailList);

    var scapDetail = scapDetailService.getLatestScapDetailByScap(scap);

    assertThat(scapDetail).contains(scapDetailList.get(1));
  }

  @Test
  void getLatestScapDetailByScapOrThrow_assertThrows() {
    when(scapDetailRepository.findAllByScap(scap)).thenReturn(Collections.emptyList());

    assertThatThrownBy(() -> scapDetailService.getLatestScapDetailByScapOrThrow(scap))
        .isInstanceOf(ScapEntityNotFoundException.class);
  }

  @Test
  void getLatestScapDetailByScapId() {
    var scapDetail = new ScapDetail();

    when(scapDetailRepository.findFirstByScapIdAndTipFlag(scap.getId(), true)).thenReturn(Optional.of(scapDetail));

    var returnedScapDetail = scapDetailService.getLatestScapDetailByScapId(scap.getScapId());

    assertThat(returnedScapDetail).contains(scapDetail);
  }

  @Test
  void getLatestScapDetailByScapIdOrThrow_IsFound_AssertReturns() {
    var scapDetail = new ScapDetail();

    when(scapDetailRepository.findFirstByScapIdAndTipFlag(scap.getId(), true)).thenReturn(Optional.of(scapDetail));

    var returnedScapDetail = scapDetailService.getLatestScapDetailByScapIdOrThrow(scap.getScapId());

    assertThat(returnedScapDetail).isEqualTo(scapDetail);
  }

  @Test
  void getLatestScapDetailByScapIdOrThrow_NotFound_AssertThrows() {
    when(scapDetailRepository.findFirstByScapIdAndTipFlag(scap.getId(), true)).thenReturn(Optional.empty());

    assertThatThrownBy(() -> scapDetailService.getLatestScapDetailByScapIdOrThrow(scap.getScapId()))
        .isInstanceOf(ScapEntityNotFoundException.class);
  }

  @Test
  void submitScap_VerifySaves() {
    var scapDetail = new ScapDetail();
    scapDetail.setStatus(ScapDetailStatus.DRAFT);
    var argumentCaptor = ArgumentCaptor.forClass(ScapDetail.class);
    var form = new ReviewAndSubmitForm();
    form.setApprovedByStakeholders(true);

    scapDetailService.submitScap(scapDetail, form);

    verify(scapDetailRepository).save(argumentCaptor.capture());

    assertThat(argumentCaptor.getValue()).extracting(
        ScapDetail::getStatus,
        ScapDetail::getSubmittedTimestamp,
        ScapDetail::getApprovedByStakeholders
    ).containsExactly(
        ScapDetailStatus.SUBMITTED,
        clock.instant(),
        true
    );
  }

  @Test
  void submitScap_VerifySaves_NotApprovedByStakeholders() {
    var scapDetail = new ScapDetail();
    scapDetail.setStatus(ScapDetailStatus.DRAFT);
    var argumentCaptor = ArgumentCaptor.forClass(ScapDetail.class);
    var form = new ReviewAndSubmitForm();
    form.setApprovedByStakeholders(false);

    scapDetailService.submitScap(scapDetail, form);

    verify(scapDetailRepository).save(argumentCaptor.capture());

    assertThat(argumentCaptor.getValue()).extracting(
        ScapDetail::getStatus,
        ScapDetail::getSubmittedTimestamp,
        ScapDetail::getApprovedByStakeholders
    ).containsExactly(
        ScapDetailStatus.SUBMITTED,
        clock.instant(),
        false
    );
  }

  @Test
  void deleteScapDetail_WhenNotFirstDraft() {
    var versionNumber = 2;
    var latestScapDetail = mock(ScapDetail.class);
    var previousScapDetail = mock(ScapDetail.class);

    doReturn(Optional.of(latestScapDetail)).when(scapDetailRepository).findFirstByScapIdAndTipFlag(scap.getId(), true);
    doReturn(versionNumber).when(latestScapDetail).getVersionNumber();
    doReturn(Optional.of(previousScapDetail)).when(scapDetailRepository).findByScapIdAndVersionNumber(scap.getId(), versionNumber - 1);

    scapDetailService.deleteScapById(scap.getScapId());

    verify(previousScapDetail).setTipFlag(true);
    verify(scapDetailRepository).save(previousScapDetail);

    verify(latestScapDetail).setVersionNumber(-1);
    verify(latestScapDetail).setTipFlag(false);
    verify(latestScapDetail).setStatus(ScapDetailStatus.DELETED);
    verify(scapDetailRepository).save(latestScapDetail);
  }

  @Test
  void deleteScapDetail_WhenFirstDraft() {
    var versionNumber = 1;
    var latestScapDetail = mock(ScapDetail.class);

    doReturn(Optional.of(latestScapDetail)).when(scapDetailRepository).findFirstByScapIdAndTipFlag(scap.getId(), true);
    doReturn(versionNumber).when(latestScapDetail).getVersionNumber();

    scapDetailService.deleteScapById(scap.getScapId());

    verify(latestScapDetail).setVersionNumber(-1);
    verify(latestScapDetail).setTipFlag(false);
    verify(latestScapDetail).setStatus(ScapDetailStatus.DELETED);
    verify(scapDetailRepository, never()).findByScapIdAndVersionNumber(any(), any());
    verify(scapDetailRepository).save(latestScapDetail);
  }

  private ServiceUserDetail getUserDetail() {
    return new ServiceUserDetail(1000L, 1000L, "Test", "Testerson", "test@test.com");
  }
}
