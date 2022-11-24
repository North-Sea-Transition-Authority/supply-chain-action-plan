package uk.co.nstauthority.scap.scap.actualtender.activity;

import com.google.common.annotations.VisibleForTesting;
import java.time.Instant;
import java.util.Objects;
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
import uk.co.nstauthority.scap.scap.actualtender.ActualTender;

@Entity
@Table(name = "actual_tender_activities")
public class ActualTenderActivity {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Integer id;

  @ManyToOne
  @JoinColumn(name = "actual_tender_id")
  private ActualTender actualTender;

  private String scopeTitle;

  private String scopeDescription;

  @Enumerated(EnumType.STRING)
  private RemunerationModel remunerationModel;

  private String remunerationModelName;

  @Enumerated(EnumType.STRING)
  private ContractStage contractStage;

  private Instant createdTimestamp;

  public ActualTenderActivity() {
  }

  @VisibleForTesting
  public ActualTenderActivity(Integer id) {
    this.id = id;
  }

  ActualTenderActivity(ActualTender actualTender, Instant createdTimestamp) {
    this.actualTender = actualTender;
    this.createdTimestamp = createdTimestamp;
  }

  public Integer getId() {
    return id;
  }

  public String getScopeTitle() {
    return scopeTitle;
  }

  public void setScopeTitle(String scopeTitle) {
    this.scopeTitle = scopeTitle;
  }

  public String getScopeDescription() {
    return scopeDescription;
  }

  public void setScopeDescription(String scopeDescription) {
    this.scopeDescription = scopeDescription;
  }

  public RemunerationModel getRemunerationModel() {
    return remunerationModel;
  }

  public void setRemunerationModel(RemunerationModel remunerationModel) {
    this.remunerationModel = remunerationModel;
  }

  public String getRemunerationModelName() {
    return remunerationModelName;
  }

  public void setRemunerationModelName(String remunerationModelName) {
    this.remunerationModelName = remunerationModelName;
  }

  public ContractStage getContractStage() {
    return contractStage;
  }

  public void setContractStage(ContractStage contractStage) {
    this.contractStage = contractStage;
  }

  Instant getCreatedTimestamp() {
    return createdTimestamp;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ActualTenderActivity that = (ActualTenderActivity) o;
    return Objects.equals(id, that.id) && Objects.equals(actualTender,
        that.actualTender) && Objects.equals(scopeTitle, that.scopeTitle) && Objects.equals(
        scopeDescription,
        that.scopeDescription) && remunerationModel == that.remunerationModel && Objects.equals(
        remunerationModelName,
        that.remunerationModelName) && contractStage == that.contractStage && Objects.equals(createdTimestamp,
        that.createdTimestamp);
  }

  @Override
  public int hashCode() {
    return Objects.hash(id, actualTender, scopeTitle, scopeDescription, remunerationModel, remunerationModelName,
        contractStage, createdTimestamp);
  }
}
