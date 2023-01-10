package uk.co.nstauthority.scap.scap.timeline;

import com.google.common.annotations.VisibleForTesting;
import java.time.Instant;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "timeline_events")
public class TimelineEvent {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  @Column(name = "timeline_id")
  private Integer id;

  @Column(name = "subject")
  @Enumerated(EnumType.STRING)
  private TimelineEventSubject timelineEventSubject;

  private Integer scapId;

  private Integer versionNumber;

  private Instant eventTime;

  private Long eventBy;

  public TimelineEvent() {
  }

  @VisibleForTesting
  public TimelineEvent(Integer id) {
    this.id = id;
  }

  public TimelineEventSubject getTimelineEventSubject() {
    return timelineEventSubject;
  }

  public void setTimelineEventSubject(TimelineEventSubject timelineEventSubject) {
    this.timelineEventSubject = timelineEventSubject;
  }

  public Integer getScapId() {
    return scapId;
  }

  public void setScapId(Integer scapId) {
    this.scapId = scapId;
  }

  public Integer getVersionNumber() {
    return versionNumber;
  }

  public void setVersionNumber(Integer versionNumber) {
    this.versionNumber = versionNumber;
  }

  public Instant getEventTime() {
    return eventTime;
  }

  public void setEventTime(Instant eventTime) {
    this.eventTime = eventTime;
  }

  public Long getEventBy() {
    return eventBy;
  }

  public void setEventBy(Long eventBy) {
    this.eventBy = eventBy;
  }
}
