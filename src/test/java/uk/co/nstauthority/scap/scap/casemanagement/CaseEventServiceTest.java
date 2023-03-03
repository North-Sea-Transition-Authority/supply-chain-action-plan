package uk.co.nstauthority.scap.scap.casemanagement;

import static java.util.Collections.emptyList;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventSubject.SCAP_CONSULTATION_REQUESTED;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventSubject.FURTHER_INFO_REQUESTED;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventSubject.FURTHER_INFO_RESPONSE;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventSubject.QA_COMMENT;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventSubject.SCAP_APPROVED;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventSubject.SCAP_CONSULTATION_RESPONSE;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventSubject.SCAP_WITHDRAWN;

import java.time.Instant;
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
import uk.co.nstauthority.scap.scap.detail.ScapDetailEntityTestUtil;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.scap.ScapEntityTestUtil;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.util.DateUtil;
import uk.co.nstauthority.scap.utils.EnergyPortalUserDtoTestUtil;

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

  @Captor
  private ArgumentCaptor<CaseEvent> timelineEventArgumentCaptor;

  @InjectMocks
  CaseEventService caseEventService;

  private static final ScapId SCAP_ID = new ScapId(11111);

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
    assertNull(result.get(2).dateOfResponse());
    assertThat(result.get(1).dateOfResponse()).isEqualTo(DateUtil.instantToString(TIME.minus(1, ChronoUnit.DAYS)));
  }

  @Test
  void recordNewEvent_createsNewTimelineEvent() {
    var user = ServiceUserDetailTestUtil.Builder().build();
    when(userDetailService.getUserDetail()).thenReturn(user);

    caseEventService.recordNewEvent(CaseEventSubject.SCAP_SUBMITTED,
        SCAP_ID,
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
  void isFurtherResponseOutstanding_noRequest() {
    when(caseEventRepository.findFirstByScapIdAndCaseEventSubjectOrderByEventTimeDesc(SCAP_ID.scapId(),
        FURTHER_INFO_REQUESTED))
        .thenReturn(Optional.empty());

    assertFalse(caseEventService.isFurtherInfoResponseOutstanding(SCAP_ID));
  }

  @Test
  void isFurtherResponseOutstanding_requestNoResponse() {
    when(caseEventRepository.findFirstByScapIdAndCaseEventSubjectOrderByEventTimeDesc(SCAP_ID.scapId(),
        FURTHER_INFO_REQUESTED))
        .thenReturn(Optional.of(getRequestEvent()));

    when(caseEventRepository.findAllByScapIdAndEventTimeAfter(SCAP_ID.scapId(),
        getRequestEvent().getEventTime()))
        .thenReturn(emptyList());

    assertTrue(caseEventService.isFurtherInfoResponseOutstanding(SCAP_ID));
  }

  @Test
  void isFurtherResponseOutstanding_requestAndResponse() {
    when(caseEventRepository.findFirstByScapIdAndCaseEventSubjectOrderByEventTimeDesc(SCAP_ID.scapId(),
        FURTHER_INFO_REQUESTED))
        .thenReturn(Optional.of(getRequestEvent()));

    when(caseEventRepository.findAllByScapIdAndEventTimeAfter(SCAP_ID.scapId(),
        getRequestEvent().getEventTime()))
        .thenReturn(getTimelineEvents());

    assertFalse(caseEventService.isFurtherInfoResponseOutstanding(SCAP_ID));
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

    assertThat(actions.get("QA")).containsExactly(QA_COMMENT);
    assertThat(actions.get("Consultations")).containsExactly(SCAP_CONSULTATION_REQUESTED, SCAP_CONSULTATION_RESPONSE);
    assertThat(actions.get("Decisions")).containsExactly(SCAP_APPROVED, SCAP_WITHDRAWN);
    assertThat(actions.get("Further Info")).containsExactly(FURTHER_INFO_REQUESTED);
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

    assertThat(actions).isEmpty();
  }

  @Test
  void getApplicableActions_isRegulatorOutstandingResponse() {
    when(caseEventRepository.findFirstByScapIdAndCaseEventSubjectOrderByEventTimeDesc(SCAP_ID.scapId(),
        FURTHER_INFO_REQUESTED))
        .thenReturn(Optional.of(getRequestEvent()));
    when(caseEventRepository.findAllByScapIdAndEventTimeAfter(SCAP_ID.scapId(),
        getRequestEvent().getEventTime()))
        .thenReturn(emptyList());

    var user = ServiceUserDetailTestUtil.Builder().build();
    when(userDetailService.getUserDetail()).thenReturn(user);
    when(teamService.userIsMemberOfRegulatorTeam(user)).thenReturn(true);
    when(scapDetailService.getLatestScapDetailByScapIdOrThrow(SCAP_ID)).thenReturn(ScapDetailEntityTestUtil.scapDetailBuilder()
        .withStatus(ScapDetailStatus.APPROVED)
        .withScap(ScapEntityTestUtil.scapBuilder().withScapId(SCAP_ID).build())
        .build());

    var actions = caseEventService.getApplicableActionsForScap(SCAP_ID);

    assertThat(actions.get("QA")).containsExactly(QA_COMMENT);
    assertThat(actions.get("Consultations")).containsExactly(SCAP_CONSULTATION_REQUESTED, SCAP_CONSULTATION_RESPONSE);
    assertThat(actions.get("Decisions")).containsExactly(SCAP_APPROVED, SCAP_WITHDRAWN);
    assertThat(actions.get("Further Info")).containsExactly(FURTHER_INFO_RESPONSE);
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

    assertThat(actions.get("Consultations")).containsExactly(SCAP_CONSULTATION_RESPONSE);
    assertThat(actions.get("Further Info")).isEmpty();
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
    when(caseEventRepository.findFirstByScapIdAndCaseEventSubjectOrderByEventTimeDesc(SCAP_ID.scapId(),
        FURTHER_INFO_REQUESTED))
        .thenReturn(Optional.of(getRequestEvent()));
    when(caseEventRepository.findAllByScapIdAndEventTimeAfter(SCAP_ID.scapId(),
        getRequestEvent().getEventTime()))
        .thenReturn(emptyList());

    var user = ServiceUserDetailTestUtil.Builder().build();
    when(userDetailService.getUserDetail()).thenReturn(user);
    when(teamService.userIsMemberOfRegulatorTeam(user)).thenReturn(false);
    when(scapDetailService.getLatestScapDetailByScapIdOrThrow(SCAP_ID)).thenReturn(ScapDetailEntityTestUtil.scapDetailBuilder()
        .withStatus(ScapDetailStatus.APPROVED)
        .build());

    var actions = caseEventService.getApplicableActionsForScap(SCAP_ID);

    assertThat(actions.get("Consultations")).containsExactly(SCAP_CONSULTATION_RESPONSE);
    assertThat(actions.get("Further Info")).containsExactly(FURTHER_INFO_RESPONSE);
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
}
