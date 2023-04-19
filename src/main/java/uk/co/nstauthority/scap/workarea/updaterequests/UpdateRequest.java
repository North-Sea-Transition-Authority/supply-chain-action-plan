package uk.co.nstauthority.scap.workarea.updaterequests;

import com.google.common.annotations.VisibleForTesting;
import java.time.LocalDate;
import java.util.UUID;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEvent;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;

@Entity
@Table(name = "scap_update_requests")
public class UpdateRequest {
  @Id
  @GeneratedValue(generator = "uuid")
  private UUID id;

  @ManyToOne
  @JoinColumn(name = "scap_detail_id")
  private ScapDetail scapDetail;

  @OneToOne
  @JoinColumn(name = "case_event_id")
  private CaseEvent caseEvent;

  @Enumerated(EnumType.STRING)
  @Column(name = "request_type")
  private UpdateRequestType updateRequestType;

  private LocalDate createdTimestamp;

  private Integer createdByUserId;

  private LocalDate dueDate;

  private LocalDate resolutionDate;

  private Integer resolvedByUserId;

  @VisibleForTesting
  public UpdateRequest(UUID uuid) {
    this.id = uuid;
  }

  public UpdateRequest() {
  }

  public UpdateRequest(ScapDetail scapDetail,
                       UpdateRequestType updateRequestType,
                       LocalDate dueDate,
                       CaseEvent caseEvent) {
    this.scapDetail = scapDetail;
    this.updateRequestType = updateRequestType;
    this.dueDate = dueDate;
    this.caseEvent = caseEvent;
  }

  public UUID getId() {
    return id;
  }

  public ScapDetail getScapDetail() {
    return scapDetail;
  }

  public void setScapDetail(ScapDetail scapDetail) {
    this.scapDetail = scapDetail;
  }

  public UpdateRequestType getUpdateRequestType() {
    return updateRequestType;
  }

  public void setUpdateRequestType(UpdateRequestType updateRequestType) {
    this.updateRequestType = updateRequestType;
  }

  public CaseEvent getCaseEvent() {
    return caseEvent;
  }

  public void setCaseEvent(CaseEvent caseEvent) {
    this.caseEvent = caseEvent;
  }

  public LocalDate getCreatedTimestamp() {
    return createdTimestamp;
  }

  public LocalDate getDueDate() {
    return dueDate;
  }

  public void setDueDate(LocalDate dueDate) {
    this.dueDate = dueDate;
  }

  public LocalDate getResolutionDate() {
    return resolutionDate;
  }

  public void setResolutionDate(LocalDate resolutionDate) {
    this.resolutionDate = resolutionDate;
  }

  public Integer getCreatedByUserId() {
    return createdByUserId;
  }

  public void setCreatedByUserId(Integer createdByUserId) {
    this.createdByUserId = createdByUserId;
  }

  public Integer getResolvedByUserId() {
    return resolvedByUserId;
  }

  public void setResolvedByUserId(Integer resolvedByUserId) {
    this.resolvedByUserId = resolvedByUserId;
  }
}
