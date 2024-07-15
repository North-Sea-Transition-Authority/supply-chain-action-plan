package uk.co.nstauthority.scap.scap.contractingperformance;

import com.google.common.annotations.VisibleForTesting;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Table;
import java.math.BigDecimal;
import java.time.Instant;
import org.hibernate.annotations.CreationTimestamp;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivity;

@Entity
@Table(name = "activity_contracting_performance")
public class ContractingPerformance {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Integer id;

  @ManyToOne
  @JoinColumn(name = "contracting_performance_overview_id")
  private ContractingPerformanceOverview contractingPerformanceOverview;

  @OneToOne
  @JoinColumn(name = "actual_tender_activity_id")
  private ActualTenderActivity actualTenderActivity;

  private BigDecimal outturnCost;

  private String outturnRationale;

  @CreationTimestamp
  private Instant createdTimestamp;

  public ContractingPerformance() {
  }

  @VisibleForTesting
  public ContractingPerformance(Integer id) {
    this.id = id;
  }

  public ContractingPerformance(ContractingPerformanceOverview contractingPerformanceOverview,
                                Instant createdTimestamp) {
    this.contractingPerformanceOverview = contractingPerformanceOverview;
    this.createdTimestamp = createdTimestamp;
  }

  public Integer getId() {
    return id;
  }

  public void setId(Integer id) {
    this.id = id;
  }

  public void setContractingPerformanceOverview(
      ContractingPerformanceOverview contractingPerformanceOverview) {
    this.contractingPerformanceOverview = contractingPerformanceOverview;
  }

  @VisibleForTesting
  ContractingPerformanceOverview getContractingPerformanceOverview() {
    return contractingPerformanceOverview;
  }

  @VisibleForTesting
  Instant getCreatedTimestamp() {
    return createdTimestamp;
  }

  public ActualTenderActivity getActualTenderActivity() {
    return actualTenderActivity;
  }

  public void setActualTenderActivity(
      ActualTenderActivity actualTenderActivity) {
    this.actualTenderActivity = actualTenderActivity;
  }

  public BigDecimal getOutturnCost() {
    return outturnCost;
  }

  public void setOutturnCost(BigDecimal outturnCost) {
    this.outturnCost = outturnCost;
  }

  public String getOutturnRationale() {
    return outturnRationale;
  }

  public void setOutturnRationale(String outturnRationale) {
    this.outturnRationale = outturnRationale;
  }
}
