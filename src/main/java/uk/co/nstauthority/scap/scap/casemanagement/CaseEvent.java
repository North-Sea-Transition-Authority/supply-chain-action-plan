package uk.co.nstauthority.scap.scap.casemanagement;

import com.google.common.annotations.VisibleForTesting;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Table;
import java.time.Instant;
import uk.co.nstauthority.scap.file.UploadedFile;

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

  private String decisionRationale;

  @OneToOne
  @JoinColumn(name = "file_id")
  private UploadedFile uploadedFile;

  public CaseEvent() {
  }

  @VisibleForTesting
  public CaseEvent(Integer id) {
    this.id = id;
  }

  public Integer getId() {
    return id;
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

  public String getDecisionRationale() {
    return decisionRationale;
  }

  public void setDecisionRationale(String decisionRationale) {
    this.decisionRationale = decisionRationale;
  }

  public UploadedFile getUploadedFile() {
    return uploadedFile;
  }

  public void setUploadedFile(UploadedFile uploadedFile) {
    this.uploadedFile = uploadedFile;
  }
}
