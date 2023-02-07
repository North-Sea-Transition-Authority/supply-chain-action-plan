package uk.co.nstauthority.scap.scap.detail;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static uk.co.nstauthority.scap.scap.detail.ScapDetailStatus.DRAFT;
import static uk.co.nstauthority.scap.scap.detail.ScapDetailStatus.SUBMITTED;

import java.time.Clock;
import java.time.Instant;
import java.time.ZoneId;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import org.assertj.core.groups.Tuple;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.authentication.ServiceUserDetail;
import uk.co.nstauthority.scap.authentication.UserDetailService;
import uk.co.nstauthority.scap.error.exception.ScapBadRequestException;
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
    var details = getListScapDetail();
    when(scapDetailRepository.findAllByScap(scap)).thenReturn(details);

    var scapDetail = scapDetailService.getLatestScapDetailByScap(scap);
    assertThat(scapDetail).contains(details.get(1));
  }

  @Test
  void getLatestScapDetailByScapOrThrow_assertReturns() {
    var details = getListScapDetail();
    when(scapDetailRepository.findAllByScap(scap)).thenReturn(details);

    var scapDetail = scapDetailService.getLatestScapDetailByScapOrThrow(scap);
    assertThat(scapDetail).isEqualTo(details.get(1));
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
  void findLatestScapDetailByScapIdAndStatus_NotFound_ReturnsEmpty() {
    when(scapDetailRepository.findFirstByScapIdAndTipFlagAndStatus(scap.getId(), true, SUBMITTED))
        .thenReturn(Optional.empty());

    assertThat(scapDetailService.findLatestSubmittedScapDetail(scap.getScapId())).isEmpty();
  }

  @Test
  void findLatestScapDetailByScapIdAndStatus_Found_Returns() {
    var scapDetail = new ScapDetail();
    when(scapDetailRepository.findFirstByScapIdAndTipFlagAndStatus(scap.getId(), true, SUBMITTED))
        .thenReturn(Optional.of(scapDetail));

    assertThat(scapDetailService.findLatestSubmittedScapDetail(scap.getScapId())).isNotEmpty();
  }

  @Test
  void getLatestScapDetailByScapIdAndStatus_NotFound_Throws() {
    when(scapDetailRepository.findFirstByScapIdAndTipFlagAndStatus(scap.getId(), true, SUBMITTED))
        .thenReturn(Optional.empty());

    assertThatThrownBy(() -> scapDetailService.getLatestSubmittedScapDetail(scap.getScapId()))
        .isInstanceOf(ScapEntityNotFoundException.class);
  }

  @Test
  void getLatestScapDetailByScapIdAndStatus_Found_Returns() {
    var scapDetail = new ScapDetail();
    when(scapDetailRepository.findFirstByScapIdAndTipFlagAndStatus(scap.getId(), true, SUBMITTED))
        .thenReturn(Optional.of(scapDetail));

    assertThat(scapDetailService.getLatestSubmittedScapDetail(scap.getScapId())).isEqualTo(scapDetail);
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
        SUBMITTED,
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
        SUBMITTED,
        clock.instant(),
        false
    );
  }

  @Test
  void approveScap_VerifySaves() {
    var scapDetail = new ScapDetail();
    scapDetail.setStatus(SUBMITTED);

    var argumentCaptor = ArgumentCaptor.forClass(ScapDetail.class);
    scapDetailService.approveScap(scapDetail);

    verify(scapDetailRepository).save(argumentCaptor.capture());
    assertThat(argumentCaptor.getValue()).extracting(
        ScapDetail::getStatus,
        ScapDetail::getApprovedTimestamp
    ).containsExactly(
        ScapDetailStatus.APPROVED,
        clock.instant()
    );
  }

  @Test
  void closeOutScap_VerifySavesAsCloseOut() {
    var scapDetail = new ScapDetail();
    scapDetail.setStatus(SUBMITTED);

    var argumentCaptor = ArgumentCaptor.forClass(ScapDetail.class);
    scapDetailService.closeOutScap(scapDetail);

    verify(scapDetailRepository).save(argumentCaptor.capture());
    assertThat(argumentCaptor.getValue()).extracting(
        ScapDetail::getStatus,
        ScapDetail::getApprovedTimestamp
    ).containsExactly(
        ScapDetailStatus.CLOSED_OUT,
        clock.instant()
    );
  }

  @Test
  void approveScap_VerifyRejects() {
    var scap = new Scap();
    scap.setReference("TEST/2023/01");

    var scapDetail = new ScapDetail();
    scapDetail.setScap(scap);
    scapDetail.setStatus(ScapDetailStatus.DRAFT);

    assertThatThrownBy(() -> scapDetailService.approveScap(scapDetail))
        .isInstanceOf(ScapBadRequestException.class);
  }

  @Test
  void closeOutScap_VerifyRejects() {
    var scap = new Scap();
    scap.setReference("TEST/2023/01");

    var scapDetail = new ScapDetail();
    scapDetail.setScap(scap);
    scapDetail.setStatus(ScapDetailStatus.DRAFT);

    assertThatThrownBy(() -> scapDetailService.closeOutScap(scapDetail))
        .isInstanceOf(ScapBadRequestException.class);
  }

  @Test
  void withdrawScap_VerifyRejects() {
    var scap = new Scap();
    scap.setReference("TEST/2023/01");

    var scapDetail = new ScapDetail();
    scapDetail.setScap(scap);
    scapDetail.setStatus(ScapDetailStatus.DRAFT);

    assertThatThrownBy(() -> scapDetailService.withdrawScap(scapDetail))
        .isInstanceOf(ScapBadRequestException.class);
  }

  @Test
  void withdrawScap_VerifyWithdraws() {
    var scap = new Scap(100);
    var scapDetail = new ScapDetail();
    scapDetail.setScap(scap);
    scapDetail.setStatus(SUBMITTED);

    scapDetailService.withdrawScap(scapDetail);
    ArgumentCaptor<List<ScapDetail>> argumentCaptor = ArgumentCaptor.forClass(List.class);

    verify(scapDetailRepository).saveAll(argumentCaptor.capture());
    assertThat(argumentCaptor.getValue()).extracting(
        ScapDetail::getStatus,
        ScapDetail::getApprovedTimestamp
    ).containsExactly(new Tuple(
        ScapDetailStatus.WITHDRAWN,
        clock.instant())
    );
  }

  @Test
  void withdrawScap_VerifyDeletesDraft() {
    var scap = new Scap(100);
    var scapDetail = new ScapDetail();
    scapDetail.setScap(scap);
    scapDetail.setStatus(SUBMITTED);

    var draftDetail = new ScapDetail();
    draftDetail.setScap(scap);
    draftDetail.setStatus(DRAFT);

    when(scapDetailRepository.findAllByScapIdAndStatus(scapDetail.getScap().getId(),
        ScapDetailStatus.DRAFT)).thenReturn(List.of(draftDetail));

    ArgumentCaptor<List<ScapDetail>> argumentCaptor = ArgumentCaptor.forClass(List.class);
    scapDetailService.withdrawScap(scapDetail);

    verify(scapDetailRepository).saveAll(argumentCaptor.capture());
    assertThat(argumentCaptor.getAllValues().get(0)).extracting(
        ScapDetail::getStatus,
        ScapDetail::getApprovedTimestamp
    ).containsExactly(new Tuple(
        ScapDetailStatus.DELETED,
        clock.instant()),
        new Tuple(
        ScapDetailStatus.WITHDRAWN,
        clock.instant())
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

  private List<ScapDetail> getListScapDetail() {
    var scapDetailList = List.of(
        new ScapDetail(scap, 1, false, ScapDetailStatus.DRAFT, EntityTestingUtil.dateToInstant(2000, 4, 23), 1),
        new ScapDetail(scap, 2, true, ScapDetailStatus.DRAFT, EntityTestingUtil.dateToInstant(2000, 4, 23), 1)
    );
    return scapDetailList;
  }
}
