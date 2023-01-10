package uk.co.nstauthority.scap.scap.timeline;

import java.time.Instant;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.authentication.UserDetailService;
import uk.co.nstauthority.scap.energyportal.EnergyPortalUserDto;
import uk.co.nstauthority.scap.energyportal.EnergyPortalUserService;
import uk.co.nstauthority.scap.energyportal.WebUserAccountId;
import uk.co.nstauthority.scap.scap.scap.ScapId;

@Service
public class TimelineEventService {

  private final UserDetailService userDetailService;

  private final EnergyPortalUserService energyPortalUserService;

  private final TimelineEventRepository timelineEventRepository;

  @Autowired
  public TimelineEventService(UserDetailService userDetailService,
                              EnergyPortalUserService energyPortalUserService,
                              TimelineEventRepository timelineEventRepository) {
    this.userDetailService = userDetailService;
    this.energyPortalUserService = energyPortalUserService;
    this.timelineEventRepository = timelineEventRepository;
  }

  public TimelineEvent recordNewEvent(TimelineEventSubject subject, ScapId scapId, Integer scapVersion) {
    var timelineEvent = new TimelineEvent();
    timelineEvent.setTimelineEventSubject(subject);
    timelineEvent.setEventTime(Instant.now());
    timelineEvent.setScapId(scapId.scapId());
    timelineEvent.setVersionNumber(scapVersion);

    var loggedInUser = userDetailService.getUserDetail();
    timelineEvent.setEventBy(loggedInUser.getWebUserAccountId().id());

    return timelineEventRepository.save(timelineEvent);
  }

  public List<TimelineEvent> getEventsByScapId(ScapId scapId) {
    return timelineEventRepository.findAllByScapId(scapId.scapId());
  }

  public List<TimelineEventView> getEventViewByScapId(ScapId scapId) {
    var timelineEvents = getEventsByScapId(scapId);
    var formatter = DateTimeFormatter.ofPattern("dd MMMM yyyy").withZone(ZoneId.systemDefault());

    var users = timelineEvents
        .stream()
        .map(TimelineEvent::getEventBy)
        .map(WebUserAccountId::new)
        .toList();

    var userDisplayNames = energyPortalUserService.findByWuaIds(users)
        .stream()
        .collect(Collectors.toMap(EnergyPortalUserDto::webUserAccountId, EnergyPortalUserDto::displayName));

    return timelineEvents
        .stream()
        .sorted(Comparator.comparing(TimelineEvent::getEventTime))
        .map(timelineEvent ->
            new TimelineEventView(
                timelineEvent.getTimelineEventSubject().getDisplayName(),
                timelineEvent.getScapId(),
                timelineEvent.getVersionNumber(),
                formatter.format(timelineEvent.getEventTime()),
                userDisplayNames.get(timelineEvent.getEventBy())
            ))
        .toList();
  }
}
