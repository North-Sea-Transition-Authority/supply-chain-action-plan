package uk.co.nstauthority.scap.scap.summary.actualtender;

import java.util.Map;
import uk.co.nstauthority.scap.scap.RemunerationModel;
import uk.co.nstauthority.scap.scap.actualtender.activity.ContractStage;
import uk.co.nstauthority.scap.scap.scap.ScapId;

public class ActualTenderActivitySummaryViewBuilder {

  private ScapId scapId;
  private Integer activityId;
  private String scopeTitle;
  private String scopeDescription;
  private RemunerationModel remunerationModel;
  private String remunerationModelName;
  private ContractStage contractStage;
  private Map<String, Boolean> ittParticipants;
  private Map<String, Boolean> bidParticipants;
  private AwardedContractSummaryView awardedContractSummaryView;
  private boolean isValid;
  
  public static ActualTenderActivitySummaryViewBuilder newBuilder() {
    return new ActualTenderActivitySummaryViewBuilder();
  }

  public ActualTenderActivitySummaryViewBuilder withScapId(ScapId scapId) {
    this.scapId = scapId;
    return this;
  }

  public ActualTenderActivitySummaryViewBuilder withActivityId(Integer activityId) {
    this.activityId = activityId;
    return this;
  }

  public ActualTenderActivitySummaryViewBuilder withScopeTitle(String scopeTitle) {
    this.scopeTitle = scopeTitle;
    return this;
  }

  public ActualTenderActivitySummaryViewBuilder withScopeDescription(String scopeDescription) {
    this.scopeDescription = scopeDescription;
    return this;
  }

  public ActualTenderActivitySummaryViewBuilder withRemunerationModel(RemunerationModel remunerationModel) {
    this.remunerationModel = remunerationModel;
    return this;
  }

  public ActualTenderActivitySummaryViewBuilder withRemunerationModelName(String remunerationModelName) {
    this.remunerationModelName = remunerationModelName;
    return this;
  }

  public ActualTenderActivitySummaryViewBuilder withContractStage(ContractStage contractStage) {
    this.contractStage = contractStage;
    return this;
  }

  public ActualTenderActivitySummaryViewBuilder withIttParticipants(Map<String, Boolean> ittParticipants) {
    this.ittParticipants = ittParticipants;
    return this;
  }

  public ActualTenderActivitySummaryViewBuilder withBidParticipants(Map<String, Boolean> bidParticipants) {
    this.bidParticipants = bidParticipants;
    return this;
  }

  public ActualTenderActivitySummaryViewBuilder withAwardedContractSummaryView(AwardedContractSummaryView awardedContractSummaryView) {
    this.awardedContractSummaryView = awardedContractSummaryView;
    return this;
  }

  public ActualTenderActivitySummaryViewBuilder withValid(boolean valid) {
    this.isValid = valid;
    return this;
  }

  public ActualTenderActivitySummaryView build() {
    return new ActualTenderActivitySummaryView(
        scapId,
        activityId,
        scopeTitle,
        scopeDescription,
        remunerationModel,
        remunerationModelName,
        contractStage,
        ittParticipants,
        bidParticipants,
        awardedContractSummaryView,
        isValid
    );
  }
}
