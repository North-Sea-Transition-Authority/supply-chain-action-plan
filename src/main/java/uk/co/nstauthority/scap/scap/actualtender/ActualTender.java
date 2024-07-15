package uk.co.nstauthority.scap.scap.actualtender;

import com.google.common.annotations.VisibleForTesting;
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
import uk.co.nstauthority.scap.scap.actualtender.summary.HasMoreActualTenderActivities;
import uk.co.nstauthority.scap.scap.copy.ScapDetailChild;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;

@Entity
@Table(name = "actual_tenders")
public class ActualTender implements ScapDetailChild {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Integer id;

  @OneToOne
  @JoinColumn(name = "scap_detail_id")
  private ScapDetail scapDetail;

  private Boolean hasActualTenders;

  @Enumerated(EnumType.STRING)
  private HasMoreActualTenderActivities hasMoreActualTenders;

  @CreationTimestamp
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

  @Override
  public Integer getId() {
    return id;
  }

  @Override
  public void setId(Integer id) {
    this.id = id;
  }

  @Override
  public ScapDetail getScapDetail() {
    return scapDetail;
  }

  @Override
  public void setScapDetail(ScapDetail scapDetail) {
    this.scapDetail = scapDetail;
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

  public HasMoreActualTenderActivities getHasMoreActualTenders() {
    return hasMoreActualTenders;
  }

  public void setHasMoreActualTenders(HasMoreActualTenderActivities hasMoreActualTenders) {
    this.hasMoreActualTenders = hasMoreActualTenders;
  }
}
