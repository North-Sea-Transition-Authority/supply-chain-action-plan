package uk.co.nstauthority.scap.application.actualtender.detail;

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
import uk.co.nstauthority.scap.application.RemunerationModel;
import uk.co.nstauthority.scap.application.actualtender.ActualTender;

@Entity
@Table(name = "actual_tender_activities")
class ActualTenderDetail {

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

  public ActualTenderDetail() {
  }

  ActualTenderDetail(ActualTender actualTender, Instant createdTimestamp) {
    this.actualTender = actualTender;
    this.createdTimestamp = createdTimestamp;
  }

  String getScopeTitle() {
    return scopeTitle;
  }

  void setScopeTitle(String scopeTitle) {
    this.scopeTitle = scopeTitle;
  }

  String getScopeDescription() {
    return scopeDescription;
  }

  void setScopeDescription(String scopeDescription) {
    this.scopeDescription = scopeDescription;
  }

  RemunerationModel getRemunerationModel() {
    return remunerationModel;
  }

  void setRemunerationModel(RemunerationModel remunerationModel) {
    this.remunerationModel = remunerationModel;
  }

  String getRemunerationModelName() {
    return remunerationModelName;
  }

  void setRemunerationModelName(String remunerationModelName) {
    this.remunerationModelName = remunerationModelName;
  }

  ContractStage getContractStage() {
    return contractStage;
  }

  void setContractStage(ContractStage contractStage) {
    this.contractStage = contractStage;
  }

  Instant getCreatedTimestamp() {
    return createdTimestamp;
  }
}
