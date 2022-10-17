package uk.co.nstauthority.scap.application.plannedtender;

import java.time.Instant;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import uk.co.nstauthority.scap.application.detail.ScapDetail;

@Entity
@Table(name = "scap_planned_tenders")
public class ScapPlannedTender {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Integer id;

  @OneToOne
  @JoinColumn(name = "scap_detail_id")
  private ScapDetail scapDetail;

  private Boolean hasPlannedTenders;

  private Instant createdTimestamp;

  public ScapPlannedTender() {
  }

  public ScapPlannedTender(ScapDetail scapDetail, Instant createdTimestamp) {
    this.scapDetail = scapDetail;
    this.createdTimestamp = createdTimestamp;
  }

  public Integer getId() {
    return id;
  }

  public ScapDetail getScapDetail() {
    return scapDetail;
  }

  public void setScapDetail(ScapDetail scapDetail) {
    this.scapDetail = scapDetail;
  }

  public Boolean getHasPlannedTenders() {
    return hasPlannedTenders;
  }

  public void setHasPlannedTenders(Boolean hasPlannedTenders) {
    this.hasPlannedTenders = hasPlannedTenders;
  }

  public Instant getCreatedTimestamp() {
    return createdTimestamp;
  }

  public void setCreatedTimestamp(Instant createdTimestamp) {
    this.createdTimestamp = createdTimestamp;
  }
}
