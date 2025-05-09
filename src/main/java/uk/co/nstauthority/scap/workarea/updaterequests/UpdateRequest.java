package uk.co.nstauthority.scap.workarea.updaterequests;

import com.google.common.annotations.VisibleForTesting;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Table;
import java.time.LocalDate;
import java.util.UUID;
import org.hibernate.annotations.UuidGenerator;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEvent;
import uk.co.nstauthority.scap.scap.scap.Scap;

@Entity
@Table(name = "scap_update_requests")
public class UpdateRequest {
  @Id
  @UuidGenerator
  private UUID id;

  @ManyToOne
  @JoinColumn(name = "scap_id")
  private Scap scap;

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

  public UpdateRequest(Scap scap,
                       UpdateRequestType updateRequestType,
                       LocalDate dueDate,
                       CaseEvent caseEvent) {
    this.scap = scap;
    this.updateRequestType = updateRequestType;
    this.dueDate = dueDate;
    this.caseEvent = caseEvent;
  }

  public UUID getId() {
    return id;
  }

  public Scap getScap() {
    return scap;
  }

  public void setScap(Scap scap) {
    this.scap = scap;
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

  public void setCreatedTimestamp(LocalDate createdTimestamp) {
    this.createdTimestamp = createdTimestamp;
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
