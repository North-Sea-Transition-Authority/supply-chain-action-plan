package uk.co.nstauthority.scap.scap.plannedtender.activity;

import com.google.common.annotations.VisibleForTesting;
import java.math.BigDecimal;
import java.time.Instant;
import java.time.LocalDate;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import org.hibernate.annotations.CreationTimestamp;
import uk.co.nstauthority.scap.scap.RemunerationModel;
import uk.co.nstauthority.scap.scap.copy.PlannedTenderChild;
import uk.co.nstauthority.scap.scap.plannedtender.PlannedTender;

@Entity
@Table(name = "planned_tender_activities")
public class PlannedTenderActivity implements PlannedTenderChild {
  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Integer id;

  @ManyToOne
  @JoinColumn(name = "planned_tender_id")
  private PlannedTender plannedTender;

  private String scopeDescription;

  private BigDecimal estimatedValue;

  @Enumerated(EnumType.STRING)
  private RemunerationModel remunerationModel;

  private String remunerationModelName;

  private String awardRationale;

  @CreationTimestamp
  private Instant createdTimestamp;

  private LocalDate expectedActualTenderStartDate;

  private LocalDate expectedContractAwardDate;

  public PlannedTenderActivity() {
  }

  @VisibleForTesting
  public PlannedTenderActivity(Integer id) {
    this.id = id;
  }

  public PlannedTenderActivity(PlannedTender plannedTender, Instant createdTimestamp) {
    this.plannedTender = plannedTender;
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
  public PlannedTender getPlannedTender() {
    return plannedTender;
  }

  @Override
  public void setPlannedTender(PlannedTender plannedTender) {
    this.plannedTender = plannedTender;
  }

  public String getScopeDescription() {
    return scopeDescription;
  }

  public void setScopeDescription(String scopeDescription) {
    this.scopeDescription = scopeDescription;
  }

  public BigDecimal getEstimatedValue() {
    return estimatedValue;
  }

  public void setEstimatedValue(BigDecimal estimatedValue) {
    this.estimatedValue = estimatedValue;
  }

  public RemunerationModel getRemunerationModel() {
    return remunerationModel;
  }

  public void setRemunerationModel(
      RemunerationModel remunerationModel) {
    this.remunerationModel = remunerationModel;
  }

  public String getRemunerationModelName() {
    return remunerationModelName;
  }

  public void setRemunerationModelName(String remunerationModelName) {
    this.remunerationModelName = remunerationModelName;
  }

  public String getAwardRationale() {
    return awardRationale;
  }

  public void setAwardRationale(String awardRationale) {
    this.awardRationale = awardRationale;
  }

  public Instant getCreatedTimestamp() {
    return createdTimestamp;
  }

  public void setCreatedTimestamp(Instant createdTimestamp) {
    this.createdTimestamp = createdTimestamp;
  }

  public LocalDate getExpectedActualTenderStartDate() {
    return expectedActualTenderStartDate;
  }

  public void setExpectedActualTenderStartDate(LocalDate expectedActualTenderStartDate) {
    this.expectedActualTenderStartDate = expectedActualTenderStartDate;
  }

  public LocalDate getExpectedContractAwardDate() {
    return expectedContractAwardDate;
  }

  public void setExpectedContractAwardDate(LocalDate expectedContractAwardDate) {
    this.expectedContractAwardDate = expectedContractAwardDate;
  }
}
