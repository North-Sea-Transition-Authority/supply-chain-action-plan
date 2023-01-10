package uk.co.nstauthority.scap.scap.timeline;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.time.Instant;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.Collections;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.authentication.ServiceUserDetailTestUtil;
import uk.co.nstauthority.scap.authentication.UserDetailService;
import uk.co.nstauthority.scap.energyportal.EnergyPortalUserService;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.utils.EnergyPortalUserDtoTestUtil;

@ExtendWith(MockitoExtension.class)
class TimelineEventServiceTest {
  @Mock
  UserDetailService userDetailService;

  @Mock
  TimelineEventRepository timelineEventRepository;

  @Mock
  EnergyPortalUserService energyPortalUserService;

  @Captor
  private ArgumentCaptor<TimelineEvent> timelineEventArgumentCaptor;

  @InjectMocks
  TimelineEventService timelineEventService;

  private static final ScapId SCAP_ID = new ScapId(11111);

  private static final Instant TIME = Instant.now();

  @Test
  void getEventsByScapId_verifyRepositoryCall() {
    timelineEventService.getEventsByScapId(SCAP_ID);
    verify(timelineEventRepository).findAllByScapId(SCAP_ID.scapId());
  }

  @Test
  void getEventViewByScapId_convertsToEventView() {
    var user = EnergyPortalUserDtoTestUtil
        .Builder()
        .withForename("TEST")
        .withSurname("SURNAME")
        .withWebUserAccountId(1000L)
        .build();


    when(timelineEventService.getEventsByScapId(SCAP_ID)).thenReturn(getTimelineEvents());
    when(energyPortalUserService.findByWuaIds(anyList())).thenReturn(Collections.singletonList(user));
    var result = timelineEventService.getEventViewByScapId(SCAP_ID);

    var formatter = DateTimeFormatter.ofPattern("dd MMMM yyyy").withZone(ZoneId.systemDefault());
    assertThat(result.get(0).timelineEventSubject()).isEqualTo(TimelineEventSubject.SCAP_SUBMITTED.getDisplayName());
    assertThat(result.get(0).scapId()).isEqualTo(SCAP_ID.scapId());
    assertThat(result.get(0).userDisplayName()).isEqualTo("TEST SURNAME");
    assertThat(result.get(0).formattedTime()).isEqualTo(formatter.format(TIME));
  }

  @Test
  void recordNewEvent_createsNewTimelineEvent() {
    var user = ServiceUserDetailTestUtil.Builder().build();
    when(userDetailService.getUserDetail()).thenReturn(user);

    timelineEventService.recordNewEvent(TimelineEventSubject.SCAP_SUBMITTED,
        SCAP_ID,
        1);

    verify(timelineEventRepository).save(timelineEventArgumentCaptor.capture());
    var timelineEvent = timelineEventArgumentCaptor.getValue();
    assertThat(timelineEvent.getEventBy()).isEqualTo(user.getWebUserAccountId().id());
    assertThat(timelineEvent.getTimelineEventSubject()).isEqualTo(TimelineEventSubject.SCAP_SUBMITTED);
    assertThat(timelineEvent.getVersionNumber()).isEqualTo(1);
    assertThat(timelineEvent.getScapId()).isEqualTo(SCAP_ID.scapId());
  }

  private List<TimelineEvent> getTimelineEvents() {
    var submissionTimelineEvent = new TimelineEvent(1);
    submissionTimelineEvent.setTimelineEventSubject(TimelineEventSubject.SCAP_SUBMITTED);
    submissionTimelineEvent.setScapId(SCAP_ID.scapId());
    submissionTimelineEvent.setVersionNumber(1);
    submissionTimelineEvent.setEventTime(TIME);
    submissionTimelineEvent.setEventBy(1000L);

    return List.of(submissionTimelineEvent);
  }

}
