package uk.co.nstauthority.scap.application.actualtender;

import com.google.common.annotations.VisibleForTesting;
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
@Table(name = "actual_tenders")
public class ActualTender {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Integer id;

  @OneToOne
  @JoinColumn(name = "scap_detail_id")
  private ScapDetail scapDetail;

  private Boolean hasActualTenders;

  private Boolean allActualTendersAdded;

  private Instant createdTimestamp;

  public ActualTender() {
  }

  @VisibleForTesting
  public ActualTender(Integer id) {
    this.id = id;
  }

  public ActualTender(ScapDetail scapDetail, Instant createdTimestamp) {
    this.scapDetail = scapDetail;
    this.createdTimestamp = createdTimestamp;
  }

  public ScapDetail getScapDetail() {
    return scapDetail;
  }

  public Instant getCreatedTimestamp() {
    return createdTimestamp;
  }

  public Boolean getHasActualTenders() {
    return hasActualTenders;
  }

  public void setHasActualTenders(Boolean hasActualTenders) {
    this.hasActualTenders = hasActualTenders;
  }

  public Boolean getAllActualTendersAdded() {
    return allActualTendersAdded;
  }

  public void setAllActualTendersAdded(Boolean allActualTendersAdded) {
    this.allActualTendersAdded = allActualTendersAdded;
  }
}
