package uk.co.nstauthority.scap.scap.actualtender.activity;

import java.time.Instant;
import uk.co.nstauthority.scap.scap.RemunerationModel;
import uk.co.nstauthority.scap.scap.actualtender.ActualTender;

public class ActualTenderActivityBuilder {

  private Integer id = null;
  private ActualTender actualTender;
  private String scopeTitle = "Test scope title";
  private String scopeDescription = "Test scope description";
  private RemunerationModel remunerationModel = RemunerationModel.LUMP_SUM;
  private String remunerationModelName;
  private ContractStage contractStage = ContractStage.INVITATION_TO_TENDER_IS_LIVE;
  private Instant createdTimestamp = Instant.now();

  public ActualTenderActivityBuilder withId(Integer id) {
    this.id = id;
    return this;
  }

  public ActualTenderActivityBuilder withActualTender(ActualTender actualTender) {
    this.actualTender = actualTender;
    return this;
  }

  public ActualTenderActivityBuilder withScopeTitle(String scopeTitle) {
    this.scopeTitle = scopeTitle;
    return this;
  }

  public ActualTenderActivityBuilder withScopeDescription(String scopeDescription) {
    this.scopeDescription = scopeDescription;
    return this;
  }

  public ActualTenderActivityBuilder withRemunerationModel(RemunerationModel remunerationModel) {
    this.remunerationModel = remunerationModel;
    return this;
  }

  public ActualTenderActivityBuilder withRemunerationModelName(String remunerationModelName) {
    this.remunerationModelName = remunerationModelName;
    return this;
  }

  public ActualTenderActivityBuilder withContractStage(ContractStage contractStage) {
    this.contractStage = contractStage;
    return this;
  }

  public ActualTenderActivityBuilder withCreatedTimestamp(Instant createdTimestamp) {
    this.createdTimestamp = createdTimestamp;
    return this;
  }

  public ActualTenderActivity build() {
    var actualTenderActivity = new ActualTenderActivity(id);
    actualTenderActivity.setActualTender(actualTender);
    actualTenderActivity.setScopeTitle(scopeTitle);
    actualTenderActivity.setScopeDescription(scopeDescription);
    actualTenderActivity.setRemunerationModel(remunerationModel);
    actualTenderActivity.setRemunerationModelName(remunerationModelName);
    actualTenderActivity.setContractStage(contractStage);
    actualTenderActivity.setCreatedTimestamp(createdTimestamp);
    return actualTenderActivity;
  }
}
