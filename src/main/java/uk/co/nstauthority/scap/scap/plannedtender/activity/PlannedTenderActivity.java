package uk.co.nstauthority.scap.scap.plannedtender.activity;

import com.google.common.annotations.VisibleForTesting;
import java.math.BigDecimal;
import java.time.Instant;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import uk.co.nstauthority.scap.scap.RemunerationModel;
import uk.co.nstauthority.scap.scap.plannedtender.PlannedTender;

@Entity
@Table(name = "planned_tender_activities")
public class PlannedTenderActivity {
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

  private Instant createdTimestamp;

  public PlannedTenderActivity() {
  }

  @VisibleForTesting
  public PlannedTenderActivity(Integer id) {
    this.id = id;
  }

  public PlannedTenderActivity(PlannedTender plannedTender, String scopeDescription, BigDecimal estimatedValue,
                               RemunerationModel remunerationModel, String remunerationModelName, String awardRationale,
                               Instant createdTimestamp) {
    this.plannedTender = plannedTender;
    this.scopeDescription = scopeDescription;
    this.estimatedValue = estimatedValue;
    this.remunerationModel = remunerationModel;
    this.remunerationModelName = remunerationModelName;
    this.awardRationale = awardRationale;
    this.createdTimestamp = createdTimestamp;
  }

  public Integer getId() {
    return id;
  }

  public PlannedTender getPlannedTender() {
    return plannedTender;
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
}
