package uk.co.nstauthority.scap.scap.casemanagement;

import com.google.common.annotations.VisibleForTesting;
import java.time.Instant;
import java.time.LocalDate;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "case_events")
public class CaseEvent {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  @Column(name = "case_event_id")
  private Integer id;

  @Column(name = "subject")
  @Enumerated(EnumType.STRING)
  private CaseEventSubject caseEventSubject;

  private Integer scapId;

  private Integer versionNumber;

  private Instant eventTime;

  @Column(name = "event_by")
  private Long eventByWuaId;
  private String comments;

  private LocalDate dueDate;

  public CaseEvent() {
  }

  @VisibleForTesting
  public CaseEvent(Integer id) {
    this.id = id;
  }

  public CaseEventSubject getTimelineEventSubject() {
    return caseEventSubject;
  }

  public void setCaseEventSubject(CaseEventSubject caseEventSubject) {
    this.caseEventSubject = caseEventSubject;
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

  public Long getEventByWuaId() {
    return eventByWuaId;
  }

  public void setEventByWuaId(Long eventByWuaId) {
    this.eventByWuaId = eventByWuaId;
  }

  public String getComments() {
    return comments;
  }

  public void setComments(String comments) {
    this.comments = comments;
  }

  public LocalDate getDueDate() {
    return dueDate;
  }

  public void setDueDate(LocalDate dueDate) {
    this.dueDate = dueDate;
  }
}
