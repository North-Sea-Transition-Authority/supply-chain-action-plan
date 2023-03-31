package uk.co.nstauthority.scap.scap.detail;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.assertj.core.api.Assertions.tuple;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static uk.co.nstauthority.scap.scap.detail.ScapDetailStatus.APPROVED;
import static uk.co.nstauthority.scap.scap.detail.ScapDetailStatus.CLOSED_OUT;
import static uk.co.nstauthority.scap.scap.detail.ScapDetailStatus.DRAFT;
import static uk.co.nstauthority.scap.scap.detail.ScapDetailStatus.SUBMITTED;

import java.time.Clock;
import java.time.Instant;
import java.time.ZoneId;
import java.util.ArrayList;
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
import uk.co.nstauthority.scap.permissionmanagement.TeamId;
import uk.co.nstauthority.scap.permissionmanagement.TeamTestUtil;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberService;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;
import uk.co.nstauthority.scap.scap.organisationgroup.OrganisationGroupForm;
import uk.co.nstauthority.scap.scap.scap.Scap;
import uk.co.nstauthority.scap.scap.scap.ScapEntityTestUtil;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.submit.ReviewAndSubmitForm;
import uk.co.nstauthority.scap.utils.EntityTestingUtil;

@ExtendWith(MockitoExtension.class)
class ScapDetailServiceTest {

  @Mock
  ScapDetailRepository scapDetailRepository;

  @Mock
  UserDetailService userDetailService;

  @Mock
  TeamService teamService;

  @Mock
  TeamMemberService teamMemberService;

  @Mock
  Clock clock = Clock.fixed(Instant.ofEpochSecond(1667576106), ZoneId.systemDefault());

  @InjectMocks
  ScapDetailService scapDetailService;

  private Scap scap;

  private ServiceUserDetail user;

  private static final int SCAP_ID = 1000;

  private static final int SCAP_DETAIL_ID = 10001;

  @BeforeEach
  void setup() {
    scap = new Scap(SCAP_ID);
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
    assertThat(scapDetail.getCreatedByUserId()).isEqualTo(getUserDetail().getWebUserAccountId().toInt());
  }

  @Test
  void createDraftScapDetail_updateSavesNewTipFlag() {
    var scapDetail = new ScapDetail(scap, 1, true, SUBMITTED, EntityTestingUtil.dateToInstant(2000, 4, 23), getUserDetail().getWebUserAccountId().toInt());

    when(userDetailService.getUserDetail()).thenReturn(getUserDetail());
    var existingStatuses = ScapDetailStatus.getReinstateableStatuses();
    when(scapDetailRepository.findFirstByScapIdAndStatusInOrderByVersionNumberDesc(scap.getScapId().scapId(), existingStatuses))
        .thenReturn(Optional.of(scapDetail));
    scapDetailService.createDraftScapDetail(scap);

    var argumentCaptor = ArgumentCaptor.forClass(ScapDetail.class);
    verify(scapDetailRepository, times(2)).save(argumentCaptor.capture());

    assertThat(argumentCaptor.getAllValues())
        .extracting(
            ScapDetail::getTipFlag,
            ScapDetail::getScap,
            ScapDetail::getStatus,
            ScapDetail::getVersionNumber,
            ScapDetail::getCreatedByUserId)
        .containsExactly(
            tuple(false, scap, SUBMITTED, 1, getUserDetail().getWebUserAccountId().toInt()),
            tuple(true, scap, DRAFT, 2, getUserDetail().getWebUserAccountId().toInt()));
  }

  @Test
  void createDraftScapDetail_updateSavesNewTipFlagOnApproved() {
    var scapDetail = new ScapDetail(scap, 1, true, APPROVED, EntityTestingUtil.dateToInstant(2000, 4, 23), getUserDetail().getWebUserAccountId().toInt());

    when(userDetailService.getUserDetail()).thenReturn(getUserDetail());
    var existingStatuses = ScapDetailStatus.getReinstateableStatuses();
    when(scapDetailRepository.findFirstByScapIdAndStatusInOrderByVersionNumberDesc(scap.getScapId().scapId(), existingStatuses))
        .thenReturn(Optional.of(scapDetail));
    scapDetailService.createDraftScapDetail(scap);

    var argumentCaptor = ArgumentCaptor.forClass(ScapDetail.class);
    verify(scapDetailRepository, times(2)).save(argumentCaptor.capture());
    verify(scapDetailRepository, never()).findFirstByScapIdAndStatusOrderByVersionNumberDesc(SCAP_ID, SUBMITTED);
    assertThat(argumentCaptor.getAllValues())
        .extracting(
            ScapDetail::getTipFlag,
            ScapDetail::getScap,
            ScapDetail::getStatus,
            ScapDetail::getVersionNumber,
            ScapDetail::getCreatedByUserId)
        .containsExactly(
            tuple(false, scap, APPROVED, 1, getUserDetail().getWebUserAccountId().toInt()),
            tuple(true, scap, DRAFT, 2, getUserDetail().getWebUserAccountId().toInt()));
  }

  @Test
  void getById_NoneFoundThrows() {
    when(scapDetailRepository.findById(SCAP_DETAIL_ID)).thenReturn(Optional.empty());
    assertThatThrownBy(() -> scapDetailService.getById(SCAP_DETAIL_ID)).isInstanceOf(ScapEntityNotFoundException.class);
  }

  @Test
  void getById_WhenFoundNotThrows() {
    var scapDetail = new ScapDetail(SCAP_DETAIL_ID);
    when(scapDetailRepository.findById(SCAP_DETAIL_ID)).thenReturn(Optional.of(scapDetail));
    assertThat(scapDetailService.getById(SCAP_DETAIL_ID)).isEqualTo(scapDetail);
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
  void listAllByScap_verifyCalls() {
    scapDetailService.findAllByScap(scap);
    verify(scapDetailRepository).findAllByScap(scap);
  }

  @Test
  void getLatestScapDetailByScapId() {
    var scapDetail = new ScapDetail();

    when(scapDetailRepository.findFirstByScapIdAndTipFlag(scap.getId(), true)).thenReturn(Optional.of(scapDetail));

    var returnedScapDetail = scapDetailService.findLatestScapDetailByScapId(scap.getScapId());

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
  void getLatestSubmittedScapDetail_NotFound_Throws() {
    when(scapDetailRepository.findFirstByScapIdAndTipFlagAndStatus(scap.getId(), true, SUBMITTED))
        .thenReturn(Optional.empty());

    assertThatThrownBy(() -> scapDetailService.getLatestSubmittedScapDetail(scap.getScapId())).isInstanceOf(ScapEntityNotFoundException.class);
  }

  @Test
  void getLatestSubmittedScapDetail_Found_Returns() {
    var scapDetail = new ScapDetail();
    when(scapDetailRepository.findFirstByScapIdAndTipFlagAndStatus(scap.getId(), true, SUBMITTED))
        .thenReturn(Optional.of(scapDetail));

    assertThat(scapDetailService.getLatestSubmittedScapDetail(scap.getScapId())).isNotNull();
  }

  @Test
  void getLatestScapDetailByScapIdAndStatus_NotFound_Throws() {
    assertThatThrownBy(() -> scapDetailService.getLatestByScapIdAndStatus(scap.getScapId(), SUBMITTED))
        .isInstanceOf(ScapEntityNotFoundException.class);
  }

  @Test
  void getLatestScapDetailByScapIdAndStatus_Found_Returns() {
    var scapDetail = new ScapDetail();
    when(scapDetailRepository.findFirstByScapIdAndStatusOrderByVersionNumberDesc(scap.getId(), SUBMITTED))
        .thenReturn(Optional.of(scapDetail));

    assertThat(scapDetailService.getLatestByScapIdAndStatus(scap.getScapId(), SUBMITTED)).isEqualTo(scapDetail);
  }

  @Test
  void getByScapIdAndVersionNumber_NotFound_Throws() {
    when(scapDetailRepository.findByScapIdAndVersionNumber(SCAP_ID, 1)).thenReturn(Optional.empty());
    assertThatThrownBy(() -> scapDetailService.getByScapIdAndVersionNumber(new ScapId(SCAP_ID), 1))
        .isInstanceOf(ScapEntityNotFoundException.class);
  }

  @Test
  void getByScapIdAndVersionNumber_Found_Returns() {
    var scapDetail = new ScapDetail(SCAP_DETAIL_ID);
    when(scapDetailRepository.findFirstByScapIdAndStatusNotInOrderByVersionNumberDesc(SCAP_ID, List.of(DRAFT))).thenReturn(Optional.of(scapDetail));
    assertThat(scapDetailService.getLatestByScapIdAndStatusNotIn(new ScapId(SCAP_ID), List.of(DRAFT))).isEqualTo(scapDetail);
  }

  @Test
  void getLatestStatusNotIn_NotFound_ReturnsEmpty() {
    when(scapDetailRepository.findFirstByScapIdAndStatusNotInOrderByVersionNumberDesc(SCAP_ID, List.of(DRAFT))).thenReturn(Optional.empty());
    assertThat(scapDetailService.findLatestByScapIdAndStatusNotIn(new ScapId(SCAP_ID), List.of(DRAFT))).isNotPresent();
  }


  @Test
  void findByScapIdAndVersionNumber_Found_Returns() {
    var scapDetail = new ScapDetail(SCAP_DETAIL_ID);
    when(scapDetailRepository.findFirstByScapIdAndStatusNotInOrderByVersionNumberDesc(SCAP_ID, List.of(DRAFT))).thenReturn(Optional.of(scapDetail));
    assertThat(scapDetailService.findLatestByScapIdAndStatusNotIn(new ScapId(SCAP_ID), List.of(DRAFT))).isEqualTo(Optional.of(scapDetail));
  }

  @Test
  void findLatestStatusNotIn_NotFound_Throws() {
    when(scapDetailRepository.findFirstByScapIdAndStatusNotInOrderByVersionNumberDesc(SCAP_ID, List.of(DRAFT))).thenReturn(Optional.empty());
    assertThatThrownBy(() -> scapDetailService.getLatestByScapIdAndStatusNotIn(new ScapId(SCAP_ID), List.of(DRAFT)))
        .isInstanceOf(ScapEntityNotFoundException.class);
  }

  @Test
  void getLatestStatusNotIn_Found_Returns() {
    var scapDetail = new ScapDetail(SCAP_DETAIL_ID);
    when(scapDetailRepository.findByScapIdAndVersionNumber(SCAP_ID, 1)).thenReturn(Optional.of(scapDetail));
    assertThat(scapDetailService.getByScapIdAndVersionNumber(new ScapId(SCAP_ID), 1)).isEqualTo(scapDetail);
  }

  @Test
  void findLatestByIdAndStatus_verifyCalls() {
    var scapId = new ScapId(SCAP_ID);
    scapDetailService.findLatestByScapIdAndStatus(scapId, DRAFT);
    verify(scapDetailRepository).findFirstByScapIdAndStatusOrderByVersionNumberDesc(scapId.scapId(), DRAFT);
  }

  @Test
  void isUpdateInProgress_NoDraft_ReturnsFalse() {
    var scapId = new ScapId(1000);
    assertFalse(scapDetailService.isUpdateInProgress(scapId));
    verify(scapDetailRepository).findFirstByScapIdAndStatusOrderByVersionNumberDesc(scapId.scapId(), DRAFT);
  }

  @Test
  void isUpdateInProgress_IntialDraft_ReturnsFalse() {
    var scapId = new ScapId(SCAP_ID
    );
    var scapDetail = new ScapDetail(SCAP_DETAIL_ID);

    //Version Number of Intial Draft scap before first submission
    scapDetail.setVersionNumber(1);
    when(scapDetailRepository.findFirstByScapIdAndStatusOrderByVersionNumberDesc(scapId.scapId(), DRAFT)).thenReturn(Optional.of(scapDetail));

    assertFalse(scapDetailService.isUpdateInProgress(scapId));
  }

  @Test
  void isUpdateInProgress_DraftUpdate_ReturnsTrue() {
    var scapId = new ScapId(SCAP_ID);
    var scapDetail = new ScapDetail(SCAP_DETAIL_ID);

    //Version Number of Intial Draft scap before first submission
    scapDetail.setVersionNumber(2);
    when(scapDetailRepository.findFirstByScapIdAndStatusOrderByVersionNumberDesc(scapId.scapId(), DRAFT)).thenReturn(Optional.of(scapDetail));

    assertTrue(scapDetailService.isUpdateInProgress(scapId));
  }

  @Test
  void getApplicableActions_userIsRegulator() {
    when(scapDetailRepository.findAllByScap(scap)).thenReturn(getListScapDetail());
    when(userDetailService.getUserDetail()).thenReturn(getUserDetail());
    when(teamService.userIsMemberOfRegulatorTeam(getUserDetail())).thenReturn(true);

    var result = scapDetailService.getAllVersionsForUser(scap);
    assertThat(result).hasSize(1);
    assertThat(result.get(0).getStatus()).isEqualTo(SUBMITTED);
  }

  @Test
  void getApplicableActions_userIsIndustry() {
    var expectedResult = getListScapDetail();
    when(scapDetailRepository.findAllByScap(scap)).thenReturn(expectedResult);
    when(userDetailService.getUserDetail()).thenReturn(getUserDetail());
    when(teamService.userIsMemberOfRegulatorTeam(getUserDetail())).thenReturn(false);

    expectedResult.remove(2);
    var result = scapDetailService.getAllVersionsForUser(scap);
    assertThat(result)
        .hasSize(3)
        .containsExactlyElementsOf(expectedResult);
  }

  @Test
  void submitScap_VerifySaves() {
    var scapDetail = new ScapDetail(SCAP_DETAIL_ID);
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
    var scap = new Scap(SCAP_ID);
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
    var scap = new Scap(SCAP_ID);
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
  void reinstateScap_verifyCalls() {
    var scap = new Scap(SCAP_ID);
    var scapDetail = new ScapDetail();
    scapDetail.setScap(scap);
    scapDetail.setVersionNumber(5);
    scapDetail.setStatus(CLOSED_OUT);

    var scapDraftDetail = new ScapDetail();
    scapDraftDetail.setScap(scap);
    scapDraftDetail.setVersionNumber(6);
    scapDraftDetail.setStatus(DRAFT);

    var existingStatuses = ScapDetailStatus.getReinstateableStatuses();
    when(userDetailService.getUserDetail()).thenReturn(getUserDetail());
    when(scapDetailRepository.findFirstByScapIdAndStatusInOrderByVersionNumberDesc(scap.getScapId().scapId(), existingStatuses))
        .thenReturn(Optional.of(scapDetail));
    when(scapDetailRepository.save(any(ScapDetail.class))).thenReturn(scapDraftDetail);
    ArgumentCaptor<ScapDetail> argumentCaptor = ArgumentCaptor.forClass(ScapDetail.class);

    scapDetailService.reinstateScap(scap);
    verify(scapDetailRepository, times(3)).save(argumentCaptor.capture());
    var resultDetail = argumentCaptor.getAllValues().get(1);
    assertThat(resultDetail.getScap().getScapId().scapId()).isEqualTo(SCAP_ID);
    assertThat(resultDetail.getVersionNumber()).isEqualTo(6);
    assertThat(resultDetail.getStatus()).isEqualTo(DRAFT);

    var resultSubmitDetail = argumentCaptor.getAllValues().get(2);
    assertThat(resultSubmitDetail.getScap().getScapId().scapId()).isEqualTo(SCAP_ID);
    assertThat(resultSubmitDetail.getStatus()).isEqualTo(SUBMITTED);
  }

  @Test
  void deleteScapDetail_WhenNotFirstDraft() {
    var versionNumber = 2;
    var latestScapDetail = mock(ScapDetail.class);
    var previousScapDetail = mock(ScapDetail.class);

    doReturn(Optional.of(latestScapDetail)).when(scapDetailRepository).findFirstByScapIdAndStatusOrderByVersionNumberDesc(scap.getId(), DRAFT);
    doReturn(versionNumber).when(latestScapDetail).getVersionNumber();
    doReturn(new Scap(1000)).when(latestScapDetail).getScap();
    doReturn(Optional.of(previousScapDetail)).when(scapDetailRepository).findByScapIdAndVersionNumber(scap.getId(), versionNumber - 1);

    scapDetailService.deleteScapById(scap.getScapId());

    verify(previousScapDetail).setTipFlag(true);
    verify(scapDetailRepository).save(previousScapDetail);

    verify(latestScapDetail).setTipFlag(false);
    verify(latestScapDetail).setStatus(ScapDetailStatus.DELETED);
    verify(scapDetailRepository).save(latestScapDetail);
  }

  @Test
  void deleteScapDetail_WhenFirstDraft() {
    var versionNumber = 1;
    var latestScapDetail = mock(ScapDetail.class);

    doReturn(Optional.of(latestScapDetail)).when(scapDetailRepository).findFirstByScapIdAndStatusOrderByVersionNumberDesc(scap.getId(), DRAFT);
    doReturn(versionNumber).when(latestScapDetail).getVersionNumber();

    scapDetailService.deleteScapById(scap.getScapId());

    verify(latestScapDetail).setTipFlag(false);
    verify(latestScapDetail).setStatus(ScapDetailStatus.DELETED);
    verify(scapDetailRepository, never()).findByScapIdAndVersionNumber(any(), any());
    verify(scapDetailRepository).save(latestScapDetail);
  }

  @Test
  void getActionableDetail_isRegulator() {
    var user = getUserDetail();
    var team = TeamTestUtil.Builder().build();
    when(teamService.getRegulatorTeam()).thenReturn(team);
    when(teamMemberService.isMemberOfTeam(new TeamId(team.getUuid()), user)).thenReturn(true);
    when(scapDetailRepository.findFirstByScapIdAndStatusNotInOrderByVersionNumberDesc(scap.getScapId().scapId(), List.of(DRAFT)))
        .thenReturn(Optional.of(getListScapDetail().get(0)));

    scapDetailService.getActionableScapDetail(scap.getScapId(), user);
    verify(scapDetailRepository).findFirstByScapIdAndStatusNotInOrderByVersionNumberDesc(scap.getScapId().scapId(), List.of(DRAFT));
  }

  @Test
  void getActionableDetail_isIndustry() {
    var team = TeamTestUtil.Builder().build();
    when(teamService.getRegulatorTeam()).thenReturn(team);
    when(teamMemberService.isMemberOfTeam(new TeamId(team.getUuid()), getUserDetail())).thenReturn(false);
    when(scapDetailRepository.findFirstByScapIdAndTipFlag(scap.getScapId().scapId(), true))
        .thenReturn(Optional.of(getListScapDetail().get(0)));

    scapDetailService.getActionableScapDetail(scap.getScapId(), getUserDetail());
    verify(scapDetailRepository).findFirstByScapIdAndTipFlag(scap.getScapId().scapId(), true);
  }

  @Test
  void setTierOneContractor() {
    var scapDetail = mock(ScapDetail.class);
    var parentScap = ScapEntityTestUtil.scapBuilder().build();
    var form = new OrganisationGroupForm();
    form.setIsTierOneContractor(true);

    scapDetailService.setTierOneContractor(scapDetail, parentScap, form);

    verify(scapDetail).setTierOneContractor(form.getIsTierOneContractor());
    verify(scapDetail).setParentScap(parentScap);
    verifyNoMoreInteractions(scapDetail);
    verify(scapDetailRepository).save(scapDetail);
  }

  private ServiceUserDetail getUserDetail() {
    return new ServiceUserDetail(1000L, 1000L, "Test", "Testerson", "test@test.com");
  }

  private List<ScapDetail> getListScapDetail() {
    var scapDetailList = new ArrayList<ScapDetail>();
    scapDetailList.addAll(List.of(
        new ScapDetail(scap, 1, false, ScapDetailStatus.SUBMITTED, EntityTestingUtil.dateToInstant(2000, 4, 23), 1),
        new ScapDetail(scap, 2, true, ScapDetailStatus.DRAFT, EntityTestingUtil.dateToInstant(2000, 4, 23), 1),
        new ScapDetail(scap, 2, true, ScapDetailStatus.DELETED, EntityTestingUtil.dateToInstant(2000, 4, 23),1),
        new ScapDetail(scap, 3, true, ScapDetailStatus.DRAFT, EntityTestingUtil.dateToInstant(2000, 4, 23), 1)
    ));
    return scapDetailList;
  }
}
