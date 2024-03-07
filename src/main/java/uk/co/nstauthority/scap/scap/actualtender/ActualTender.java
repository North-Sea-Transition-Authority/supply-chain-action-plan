package uk.co.nstauthority.scap.scap.actualtender;

import com.google.common.annotations.VisibleForTesting;
import java.time.Instant;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import javax.persistence.Table;
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
