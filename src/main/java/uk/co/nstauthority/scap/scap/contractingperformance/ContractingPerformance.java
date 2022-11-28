package uk.co.nstauthority.scap.scap.contractingperformance;

import com.google.common.annotations.VisibleForTesting;
import java.math.BigDecimal;
import java.time.Instant;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToOne;
import javax.persistence.Table;
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
