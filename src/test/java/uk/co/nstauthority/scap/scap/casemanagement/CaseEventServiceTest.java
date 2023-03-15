package uk.co.nstauthority.scap.scap.casemanagement;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventSubject.FURTHER_INFO_REQUESTED;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventSubject.FURTHER_INFO_RESPONSE;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventSubject.QA_COMMENT;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventSubject.SCAP_APPROVED;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventSubject.SCAP_CONSULTATION_REQUESTED;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventSubject.SCAP_CONSULTATION_RESPONSE;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventSubject.SCAP_REINSTATED;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventSubject.SCAP_UPDATE_REQUESTED;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventSubject.SCAP_WITHDRAWN;

import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.fivium.formlibrary.validator.date.DateUtils;
import uk.co.nstauthority.scap.authentication.ServiceUserDetailTestUtil;
import uk.co.nstauthority.scap.authentication.UserDetailService;
import uk.co.nstauthority.scap.energyportal.EnergyPortalUserService;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.detail.ScapDetailEntityTestUtil;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.scap.Scap;
import uk.co.nstauthority.scap.scap.scap.ScapEntityTestUtil;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.util.DateUtil;
import uk.co.nstauthority.scap.utils.EnergyPortalUserDtoTestUtil;
import uk.co.nstauthority.scap.workarea.updaterequests.UpdateRequestService;
import uk.co.nstauthority.scap.workarea.updaterequests.UpdateRequestType;

@ExtendWith(MockitoExtension.class)
class CaseEventServiceTest {
  @Mock
  UserDetailService userDetailService;

  @Mock
  CaseEventRepository caseEventRepository;

  @Mock
  EnergyPortalUserService energyPortalUserService;

  @Mock
  ScapDetailService scapDetailService;

  @Mock
  TeamService teamService;

  @Mock
  UpdateRequestService updateRequestService;

  @Captor
  private ArgumentCaptor<CaseEvent> timelineEventArgumentCaptor;

  @InjectMocks
  CaseEventService caseEventService;

  private static final ScapId SCAP_ID = new ScapId(11111);

  private static final ScapDetail SCAP_DETAIL = new ScapDetail(SCAP_ID);

  private static final Instant TIME = Instant.now();

  @Test
  void getEventsByScapId_verifyRepositoryCall() {
    caseEventService.getEventsByScapId(SCAP_ID);
    verify(caseEventRepository).findAllByScapId(SCAP_ID.scapId());
  }

  @Test
  void getEventViewByScapId_convertsToEventView() {
    var user = EnergyPortalUserDtoTestUtil
        .Builder()
        .withForename("TEST")
        .withSurname("SURNAME")
        .withWebUserAccountId(1000L)
        .build();

    var events = getTimelineEvents();
    events.add(getRequestEvent());
    when(caseEventService.getEventsByScapId(SCAP_ID)).thenReturn(events);
    when(energyPortalUserService.findByWuaIds(anyList())).thenReturn(Collections.singletonList(user));
    when(caseEventRepository.findAllByScapIdAndEventTimeAfter(eq(SCAP_ID.scapId()),
        any(Instant.class)))
        .thenReturn(getTimelineEvents());
    var result = caseEventService.getEventViewByScapId(SCAP_ID);

    var formatter = DateTimeFormatter.ofPattern(DateUtils.SHORT_DATE).withZone(ZoneId.systemDefault());
    assertThat(result.get(2).caseEventSubject()).isEqualTo(CaseEventSubject.SCAP_SUBMITTED.getDisplayName());
    assertThat(result.get(2).scapId()).isEqualTo(SCAP_ID.scapId());
    assertThat(result.get(2).userDisplayName()).isEqualTo("TEST SURNAME");
    assertThat(result.get(2).formattedTime()).isEqualTo(formatter.format(TIME.minus(1, ChronoUnit.DAYS)));
    assertThat(result.get(2).dateOfResponse()).isEqualTo(formatter.format(TIME.minus(1, ChronoUnit.DAYS)));
    assertThat(result.get(1).dateOfResponse()).isEqualTo(DateUtil.instantToString(TIME.minus(1, ChronoUnit.DAYS)));
  }

  @Test
  void recordNewEvent_createsNewTimelineEvent() {
    SCAP_DETAIL.setScap(new Scap(SCAP_ID));
    var user = ServiceUserDetailTestUtil.Builder().build();
    when(userDetailService.getUserDetail()).thenReturn(user);

    caseEventService.recordNewEvent(CaseEventSubject.SCAP_SUBMITTED,
        SCAP_DETAIL,
        1,
        null);

    verify(caseEventRepository).save(timelineEventArgumentCaptor.capture());
    var timelineEvent = timelineEventArgumentCaptor.getValue();
    assertThat(timelineEvent.getEventByWuaId()).isEqualTo(user.getWebUserAccountId().id());
    assertThat(timelineEvent.getTimelineEventSubject()).isEqualTo(CaseEventSubject.SCAP_SUBMITTED);
    assertThat(timelineEvent.getVersionNumber()).isEqualTo(1);
    assertThat(timelineEvent.getScapId()).isEqualTo(SCAP_ID.scapId());
  }



  @Test
  void getApplicableActions_isRegulator() {
    var user = ServiceUserDetailTestUtil.Builder().build();
    when(userDetailService.getUserDetail()).thenReturn(user);
    when(teamService.userIsMemberOfRegulatorTeam(user)).thenReturn(true);
    when(scapDetailService.getLatestScapDetailByScapIdOrThrow(SCAP_ID)).thenReturn(ScapDetailEntityTestUtil.scapDetailBuilder()
        .withStatus(ScapDetailStatus.APPROVED)
        .withScap(ScapEntityTestUtil.scapBuilder().withScapId(SCAP_ID).build())
        .build());

    var actions = caseEventService.getApplicableActionsForScap(SCAP_ID);

    assertThat(actions.get(CaseEventGroups.QA.getDisplayName())).containsExactly(QA_COMMENT);
    assertThat(actions.get(CaseEventGroups.CONSULTATIONS.getDisplayName())).containsExactly(SCAP_CONSULTATION_REQUESTED, SCAP_CONSULTATION_RESPONSE);
    assertThat(actions.get(CaseEventGroups.DECISIONS.getDisplayName())).containsExactly(SCAP_APPROVED, SCAP_WITHDRAWN);
    assertThat(actions.get(CaseEventGroups.FURTHER_INFO.getDisplayName())).containsExactly(FURTHER_INFO_REQUESTED, SCAP_UPDATE_REQUESTED);
  }

  @Test
  void getApplicableActions_isWithdrawnRegulator() {
    var user = ServiceUserDetailTestUtil.Builder().build();
    when(userDetailService.getUserDetail()).thenReturn(user);
    when(teamService.userIsMemberOfRegulatorTeam(user)).thenReturn(true);
    when(scapDetailService.getLatestScapDetailByScapIdOrThrow(SCAP_ID)).thenReturn(ScapDetailEntityTestUtil.scapDetailBuilder()
        .withStatus(ScapDetailStatus.WITHDRAWN)
        .withScap(ScapEntityTestUtil.scapBuilder().withScapId(SCAP_ID).build())
        .build());

    var actions = caseEventService.getApplicableActionsForScap(SCAP_ID);

    assertThat(actions.get(CaseEventGroups.DECISIONS.getDisplayName())).containsExactly(SCAP_REINSTATED);
  }

  @Test
  void getApplicableActions_isRegulatorOutstandingResponse() {
    when(updateRequestService.getUpdateDueDate(SCAP_ID, UpdateRequestType.FURTHER_INFORMATION)).thenReturn(
        Optional.of(LocalDate.now()));

    var user = ServiceUserDetailTestUtil.Builder().build();
    when(userDetailService.getUserDetail()).thenReturn(user);
    when(teamService.userIsMemberOfRegulatorTeam(user)).thenReturn(true);
    when(scapDetailService.getLatestScapDetailByScapIdOrThrow(SCAP_ID)).thenReturn(ScapDetailEntityTestUtil.scapDetailBuilder()
        .withStatus(ScapDetailStatus.APPROVED)
        .withScap(ScapEntityTestUtil.scapBuilder().withScapId(SCAP_ID).build())
        .build());

    var actions = caseEventService.getApplicableActionsForScap(SCAP_ID);

    assertThat(actions.get(CaseEventGroups.QA.getDisplayName())).containsExactly(QA_COMMENT);
    assertThat(actions.get(CaseEventGroups.CONSULTATIONS.getDisplayName())).containsExactly(SCAP_CONSULTATION_REQUESTED, SCAP_CONSULTATION_RESPONSE);
    assertThat(actions.get(CaseEventGroups.DECISIONS.getDisplayName())).containsExactly(SCAP_APPROVED, SCAP_WITHDRAWN);
    assertThat(actions.get(CaseEventGroups.FURTHER_INFO.getDisplayName())).containsExactly(FURTHER_INFO_RESPONSE);
  }

  @Test
  void getApplicableActions_isIndustry() {
    var user = ServiceUserDetailTestUtil.Builder().build();
    when(userDetailService.getUserDetail()).thenReturn(user);
    when(teamService.userIsMemberOfRegulatorTeam(user)).thenReturn(false);
    when(scapDetailService.getLatestScapDetailByScapIdOrThrow(SCAP_ID)).thenReturn(ScapDetailEntityTestUtil.scapDetailBuilder()
        .withStatus(ScapDetailStatus.APPROVED)
        .build());

    var actions = caseEventService.getApplicableActionsForScap(SCAP_ID);

    assertThat(actions.get(CaseEventGroups.CONSULTATIONS.getDisplayName())).containsExactly(SCAP_CONSULTATION_RESPONSE);
    assertThat(actions.get(CaseEventGroups.FURTHER_INFO.getDisplayName())).isEmpty();
  }

  @Test
  void getApplicableActions_isClosedIndustry() {
    var user = ServiceUserDetailTestUtil.Builder().build();
    when(userDetailService.getUserDetail()).thenReturn(user);
    when(teamService.userIsMemberOfRegulatorTeam(user)).thenReturn(false);
    when(scapDetailService.getLatestScapDetailByScapIdOrThrow(SCAP_ID)).thenReturn(ScapDetailEntityTestUtil.scapDetailBuilder()
        .withStatus(ScapDetailStatus.CLOSED_OUT)
        .build());

    var actions = caseEventService.getApplicableActionsForScap(SCAP_ID);
    assertThat(actions).isEmpty();
  }

  @Test
  void getApplicableActions_isIndustryRequestOutstanding() {
    when(updateRequestService.getUpdateDueDate(SCAP_ID, UpdateRequestType.FURTHER_INFORMATION))
        .thenReturn(Optional.of(LocalDate.now()));

    var user = ServiceUserDetailTestUtil.Builder().build();
    when(userDetailService.getUserDetail()).thenReturn(user);
    when(teamService.userIsMemberOfRegulatorTeam(user)).thenReturn(false);
    when(scapDetailService.getLatestScapDetailByScapIdOrThrow(SCAP_ID)).thenReturn(ScapDetailEntityTestUtil.scapDetailBuilder()
        .withStatus(ScapDetailStatus.APPROVED)
        .build());

    var actions = caseEventService.getApplicableActionsForScap(SCAP_ID);

    assertThat(actions.get(CaseEventGroups.CONSULTATIONS.getDisplayName())).containsExactly(SCAP_CONSULTATION_RESPONSE);
    assertThat(actions.get(CaseEventGroups.FURTHER_INFO.getDisplayName())).containsExactly(FURTHER_INFO_RESPONSE);
  }

  private List<CaseEvent> getTimelineEvents() {
    var submissionTimelineEvent = new CaseEvent(1);
    submissionTimelineEvent.setCaseEventSubject(CaseEventSubject.SCAP_SUBMITTED);
    submissionTimelineEvent.setScapId(SCAP_ID.scapId());
    submissionTimelineEvent.setVersionNumber(1);
    submissionTimelineEvent.setEventTime(TIME.minus(1, ChronoUnit.DAYS));
    submissionTimelineEvent.setEventByWuaId(1000L);

    var furtherInfoResponse = new CaseEvent(2);
    furtherInfoResponse.setCaseEventSubject(CaseEventSubject.FURTHER_INFO_RESPONSE);
    furtherInfoResponse.setScapId(SCAP_ID.scapId());
    furtherInfoResponse.setVersionNumber(1);
    furtherInfoResponse.setEventTime(TIME.plus(5, ChronoUnit.DAYS));
    furtherInfoResponse.setEventByWuaId(1000L);

    var list = new ArrayList<CaseEvent>();
    list.add(submissionTimelineEvent);
    list.add(furtherInfoResponse);
    return list;
  }

  private CaseEvent getRequestEvent() {
    var infoRequestTimelineEvent = new CaseEvent(15);
    infoRequestTimelineEvent.setCaseEventSubject(FURTHER_INFO_REQUESTED);
    infoRequestTimelineEvent.setScapId(SCAP_ID.scapId());
    infoRequestTimelineEvent.setVersionNumber(1);
    infoRequestTimelineEvent.setEventTime(TIME);
    infoRequestTimelineEvent.setEventByWuaId(1000L);

    return infoRequestTimelineEvent;
  }

  private CaseEvent getUpdateRequestEvent() {
    var infoRequestTimelineEvent = new CaseEvent(15);
    infoRequestTimelineEvent.setCaseEventSubject(SCAP_UPDATE_REQUESTED);
    infoRequestTimelineEvent.setScapId(SCAP_ID.scapId());
    infoRequestTimelineEvent.setVersionNumber(1);
    infoRequestTimelineEvent.setEventTime(TIME);
    infoRequestTimelineEvent.setEventByWuaId(1000L);

    return infoRequestTimelineEvent;
  }
}
