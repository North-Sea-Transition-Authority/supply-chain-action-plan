package uk.co.nstauthority.scap.scap.plannedtender;

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
import org.hibernate.annotations.CreationTimestamp;
import uk.co.nstauthority.scap.scap.copy.ScapDetailChild;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;

@Entity
@Table(name = "planned_tenders")
public class PlannedTender implements ScapDetailChild {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Integer id;

  @OneToOne
  @JoinColumn(name = "scap_detail_id")
  private ScapDetail scapDetail;

  private Boolean hasPlannedTenders;

  @CreationTimestamp
  private Instant createdTimestamp;

  @Enumerated(EnumType.STRING)
  private HasMorePlannedTenderActivities hasMorePlannedTenderActivities;

  public PlannedTender() {
  }

  public PlannedTender(ScapDetail scapDetail, Instant createdTimestamp) {
    this.scapDetail = scapDetail;
    this.createdTimestamp = createdTimestamp;
  }

  public Integer getId() {
    return id;
  }

  public ScapDetail getScapDetail() {
    return scapDetail;
  }

  @Override
  public void setId(Integer id) {
    this.id = id;
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

  public HasMorePlannedTenderActivities getHasMorePlannedTenderActivities() {
    return hasMorePlannedTenderActivities;
  }

  public void setHasMorePlannedTenderActivities(HasMorePlannedTenderActivities hasMorePlannedTenderActivities) {
    this.hasMorePlannedTenderActivities = hasMorePlannedTenderActivities;
  }
}
