package uk.co.nstauthority.scap.scap.contractingperformance;

import com.google.common.annotations.VisibleForTesting;
import java.time.Instant;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;

@Entity
@Table(name = "contracting_performance_overviews")
public class ContractingPerformanceOverview {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Integer id;

  @OneToOne
  @JoinColumn(name = "scap_detail_id")
  private ScapDetail scapDetail;

  private Boolean hasContractingPerformance;

  private Instant createdTimestamp;

  public ContractingPerformanceOverview() {
  }

  @VisibleForTesting
  ContractingPerformanceOverview(Integer id) {
    this.id = id;
  }

  public ContractingPerformanceOverview(ScapDetail scapDetail, Instant createdTimestamp) {
    this.scapDetail = scapDetail;
    this.createdTimestamp = createdTimestamp;
  }

  Integer getId() {
    return id;
  }

  @VisibleForTesting
  ScapDetail getScapDetail() {
    return scapDetail;
  }

  @VisibleForTesting
  Instant getCreatedTimestamp() {
    return createdTimestamp;
  }

  public Boolean getHasContractingPerformance() {
    return hasContractingPerformance;
  }

  public void setHasContractingPerformance(Boolean hasContractingPerformance) {
    this.hasContractingPerformance = hasContractingPerformance;
  }
}
