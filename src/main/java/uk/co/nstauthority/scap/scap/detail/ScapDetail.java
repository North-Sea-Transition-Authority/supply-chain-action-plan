package uk.co.nstauthority.scap.scap.detail;

import com.google.common.annotations.VisibleForTesting;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Table;
import java.time.Instant;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.envers.Audited;
import uk.co.fivium.digitalnotificationlibrary.core.notification.DomainReference;
import uk.co.nstauthority.scap.scap.scap.Scap;
import uk.co.nstauthority.scap.scap.scap.ScapId;

@Entity
@Table(name = "scap_details")
public class ScapDetail implements DomainReference {
  @Id
  @Audited
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Integer id;

  @ManyToOne
  @JoinColumn(name = "scap_id")
  private Scap scap;

  private Integer versionNumber;

  @Enumerated(EnumType.STRING)
  @Audited
  private ScapDetailStatus status;

  @Audited
  @CreationTimestamp
  private Instant createdTimestamp;

  private Integer createdByUserId;

  @Audited
  private Instant submittedTimestamp;

  private Instant approvedTimestamp;

  private Boolean approvedByStakeholders;

  private Boolean tierOneContractor;

  @OneToOne
  @JoinColumn(name = "parent_scap_id")
  private Scap parentScap;

  public ScapDetail() {

  }

  @VisibleForTesting
  public ScapDetail(Integer id) {
    this.id = id;
  }

  @VisibleForTesting
  public ScapDetail(ScapId id) {
    this.id = id.scapId();
  }

  public ScapDetail(Scap scap, Integer versionNumber, ScapDetailStatus status,
                    Instant createdTimestamp, Integer createdByUserId) {
    this.scap = scap;
    this.createdTimestamp = createdTimestamp;
    this.versionNumber = versionNumber;
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

  @VisibleForTesting
  public void setCreatedByUserId(Integer createdByUserId) {
    this.createdByUserId = createdByUserId;
  }

  public Instant getSubmittedTimestamp() {
    return submittedTimestamp;
  }

  public void setSubmittedTimestamp(Instant submittedTimestamp) {
    this.submittedTimestamp = submittedTimestamp;
  }

  public Instant getApprovedTimestamp() {
    return approvedTimestamp;
  }

  public void setApprovedTimestamp(Instant approvedTimestamp) {
    this.approvedTimestamp = approvedTimestamp;
  }

  public Boolean getApprovedByStakeholders() {
    return approvedByStakeholders;
  }

  public void setApprovedByStakeholders(Boolean approvedByStakeholders) {
    this.approvedByStakeholders = approvedByStakeholders;
  }

  public Boolean isTierOneContractor() {
    return tierOneContractor;
  }

  public void setTierOneContractor(Boolean tierOneContractor) {
    this.tierOneContractor = tierOneContractor;
  }

  public Scap getParentScap() {
    return parentScap;
  }

  public void setParentScap(Scap parentScap) {
    this.parentScap = parentScap;
  }

  @Override
  public String getDomainId() {
    return String.valueOf(id);
  }

  @Override
  public String getDomainType() {
    return "SCAP_DETAIL";
  }
}
