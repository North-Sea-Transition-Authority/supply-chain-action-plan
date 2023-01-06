package uk.co.nstauthority.scap.scap.detail;

import com.google.common.annotations.VisibleForTesting;
import java.time.Instant;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import uk.co.nstauthority.scap.scap.scap.Scap;

@Entity
@Table(name = "scap_details")
public class ScapDetail {
  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Integer id;

  @ManyToOne
  @JoinColumn(name = "scap_id")
  private Scap scap;

  private Integer versionNumber;

  private Boolean tipFlag;

  @Enumerated(EnumType.STRING)
  private ScapDetailStatus status;

  private Instant createdTimestamp;

  private Integer createdByUserId;

  private Instant submittedTimestamp;

  public ScapDetail() {

  }

  @VisibleForTesting
  public ScapDetail(Integer id) {
    this.id = id;
  }

  public ScapDetail(Scap scap, Integer versionNumber, Boolean tipFlag, ScapDetailStatus status,
                    Instant createdTimestamp, Integer createdByUserId) {
    this.scap = scap;
    this.createdTimestamp = createdTimestamp;
    this.versionNumber = versionNumber;
    this.tipFlag = tipFlag;
    this.status = status;
    this.createdByUserId = createdByUserId;
  }

  public Integer getId() {
    return id;
  }

  public Scap getScap() {
    return scap;
  }

  public void setScap(Scap scap) {
    this.scap = scap;
  }

  public Integer getVersionNumber() {
    return versionNumber;
  }

  public void setVersionNumber(Integer versionNumber) {
    this.versionNumber = versionNumber;
  }

  public Boolean getTipFlag() {
    return tipFlag;
  }

  public void setTipFlag(Boolean tipFlag) {
    this.tipFlag = tipFlag;
  }

  public ScapDetailStatus getStatus() {
    return status;
  }

  public void setStatus(ScapDetailStatus status) {
    this.status = status;
  }

  public Instant getCreatedTimestamp() {
    return createdTimestamp;
  }

  public void setCreatedTimestamp(Instant createdTimestamp) {
    this.createdTimestamp = createdTimestamp;
  }

  public Integer getCreatedByUserId() {
    return createdByUserId;
  }

  public Instant getSubmittedTimestamp() {
    return submittedTimestamp;
  }

  public void setSubmittedTimestamp(Instant submittedTimestamp) {
    this.submittedTimestamp = submittedTimestamp;
  }
}
