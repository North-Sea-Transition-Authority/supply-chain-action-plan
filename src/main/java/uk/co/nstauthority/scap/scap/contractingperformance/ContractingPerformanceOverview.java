package uk.co.nstauthority.scap.scap.contractingperformance;

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
import uk.co.nstauthority.scap.scap.contractingperformance.summary.HasMoreContractingPerformance;
import uk.co.nstauthority.scap.scap.copy.ScapDetailChild;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;

@Entity
@Table(name = "contracting_performance_overviews")
public class ContractingPerformanceOverview implements ScapDetailChild {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Integer id;

  @OneToOne
  @JoinColumn(name = "scap_detail_id")
  private ScapDetail scapDetail;

  private Boolean hasContractingPerformance;

  @Enumerated(EnumType.STRING)
  private HasMoreContractingPerformance hasMoreContractingPerformance;

  @CreationTimestamp
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

  public HasMoreContractingPerformance getHasMoreContractingPerformance() {
    return hasMoreContractingPerformance;
  }

  public void setHasMoreContractingPerformance(
      HasMoreContractingPerformance hasMoreContractingPerformance) {
    this.hasMoreContractingPerformance = hasMoreContractingPerformance;
  }
}
