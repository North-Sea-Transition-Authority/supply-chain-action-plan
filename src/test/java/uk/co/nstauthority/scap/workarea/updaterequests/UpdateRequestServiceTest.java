package uk.co.nstauthority.scap.workarea.updaterequests;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.groups.Tuple.tuple;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.time.Clock;
import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.authentication.ServiceUserDetail;
import uk.co.nstauthority.scap.authentication.ServiceUserDetailTestUtil;
import uk.co.nstauthority.scap.authentication.UserDetailService;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEvent;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventSubject;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.detail.ScapDetailEntityTestUtil;
import uk.co.nstauthority.scap.scap.scap.Scap;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.scap.ScapService;

@ExtendWith(MockitoExtension.class)
class UpdateRequestServiceTest {

  @Mock
  UpdateRequestRepository updateRequestRepository;

  @Mock
  UserDetailService userDetailService;

  @Mock
  ScapService scapService;

  private Clock clock =  Clock.fixed(Instant.ofEpochSecond(1667576106), ZoneId.systemDefault());

  @InjectMocks
  UpdateRequestService updateRequestService;

  private static final ScapId SCAP_ID = new ScapId(1000);

  private static final Integer USER_ID = 5000;

  private ScapDetail scapDetail;

  private Scap scap;

  private ServiceUserDetail user;

  @Captor
  private ArgumentCaptor<List<UpdateRequest>> updateRequestCaptor;

  @BeforeEach
  void setup() {
    updateRequestService = new UpdateRequestService(
        updateRequestRepository,
        scapService,
        userDetailService,
        clock);

    user = ServiceUserDetailTestUtil.Builder()
        .withWuaId(Long.valueOf(USER_ID))
        .build();

    scap = new Scap(1111);

    scapDetail = ScapDetailEntityTestUtil.scapDetailBuilder()
        .withScap(scap)
        .withScapDetailId(1000)
        .build();
  }

  @Test
  void getAllByScapId_verifyCalls() {
    when(scapService.getScapById(SCAP_ID)).thenReturn(scap);
    updateRequestService.findAllByScapId(SCAP_ID);

    verify(updateRequestRepository).findAllByScap(scap);
  }

  @Test
  void createUpdateRequest_verifyCalls() {
    when(userDetailService.getUserDetail()).thenReturn(user);
    var argumentCaptor = ArgumentCaptor.forClass(UpdateRequest.class);
    var localDate = LocalDate.now();
    var caseEvent = new CaseEvent();
    updateRequestService.createUpdateRequest(scapDetail, UpdateRequestType.FURTHER_INFORMATION, localDate, caseEvent);
    verify(updateRequestRepository).save(argumentCaptor.capture());

    assertThat(argumentCaptor.getValue())
        .extracting(UpdateRequest::getUpdateRequestType,
            UpdateRequest::getDueDate,
            UpdateRequest::getCreatedTimestamp,
            UpdateRequest::getScap,
            UpdateRequest::getCreatedByUserId)
        .contains(
            UpdateRequestType.FURTHER_INFORMATION,
            localDate,
            localDate,
            scap,
            USER_ID);
  }

  @Test
  void resolveUpdateRequest_actionResolvesNoRequests() {
    when(userDetailService.getUserDetail()).thenReturn(user);
    updateRequestService.resolveUpdateRequest(scapDetail, CaseEventSubject.SCAP_CONSULTATION_RESPONSE);
    verify(updateRequestRepository)
        .findByScapAndUpdateRequestTypeInAndResolutionDateNull(scap, Collections.emptyList());
    verify(updateRequestRepository)
        .saveAll(Collections.emptyList());
  }

  @Test
  void resolveUpdateRequest_actionResolvesRequests() {
    when(userDetailService.getUserDetail()).thenReturn(user);
    var updateRequest = new UpdateRequest(UUID.randomUUID());
    updateRequest.setUpdateRequestType(UpdateRequestType.FURTHER_INFORMATION);
    updateRequest.setScap(scap);
    when(updateRequestRepository.findByScapAndUpdateRequestTypeInAndResolutionDateNull(scap,
        List.of( UpdateRequestType.FURTHER_INFORMATION, UpdateRequestType.UPDATE)))
        .thenReturn(List.of(updateRequest));

    updateRequestService.resolveUpdateRequest(scapDetail, CaseEventSubject.SCAP_SUBMITTED);
    verify(updateRequestRepository)
        .findByScapAndUpdateRequestTypeInAndResolutionDateNull(scap,
            List.of(UpdateRequestType.FURTHER_INFORMATION, UpdateRequestType.UPDATE));

    verify(updateRequestRepository)
        .saveAll(updateRequestCaptor.capture());
    assertThat(updateRequestCaptor.getValue())
        .extracting(UpdateRequest::getUpdateRequestType,
            UpdateRequest::getScap,
            UpdateRequest::getResolvedByUserId)
        .contains(tuple(
            UpdateRequestType.FURTHER_INFORMATION,
            scap,
            USER_ID));
    assertThat(updateRequestCaptor.getValue().get(0).getResolutionDate()).isNotNull();
  }

  @Test
  void getUpdateDueDate_noRequests() {
    assertThat(updateRequestService.getUpdateDueDate(SCAP_ID, UpdateRequestType.FURTHER_INFORMATION)).isEmpty();
  }

  @Test
  void getUpdateDueDate_RequestOutstanding() {
    var dueDate = LocalDate.now().plusDays(5);
    var updateRequest = new UpdateRequest(UUID.randomUUID());
    updateRequest.setUpdateRequestType(UpdateRequestType.FURTHER_INFORMATION);
    updateRequest.setScap(scap);
    updateRequest.setDueDate(dueDate);

    when(scapService.getScapById(SCAP_ID)).thenReturn(scap);
    when(updateRequestRepository
        .findFirstByScapAndResolutionDateNullAndUpdateRequestTypeOrderByCreatedTimestampDesc(scap, UpdateRequestType.FURTHER_INFORMATION))
        .thenReturn(Optional.of(updateRequest));
    assertThat(updateRequestService.getUpdateDueDate(SCAP_ID, UpdateRequestType.FURTHER_INFORMATION)).contains(dueDate);
  }

  @Test
  void findNextDueUpdate_noRequests() {
    assertThat(updateRequestService.findNextDueUpdate(SCAP_ID)).isEmpty();
  }

  @Test
  void findNextDueUpdate_RequestOutstanding() {
    var updateRequest = new UpdateRequest(UUID.randomUUID());
    var newCaseEvent = new CaseEvent();
    updateRequest.setCaseEvent(newCaseEvent);
    updateRequest.setUpdateRequestType(UpdateRequestType.FURTHER_INFORMATION);
    updateRequest.setScap(scap);

    when(scapService.getScapById(SCAP_ID)).thenReturn(scap);
    when(updateRequestRepository
        .findFirstByScapAndResolutionDateNullOrderByCreatedTimestampDesc(scap))
        .thenReturn(Optional.of(updateRequest));
    assertThat(updateRequestService.findNextDueUpdate(SCAP_ID)).contains(updateRequest);
  }
}
