package uk.co.nstauthority.scap.application.plannedtender.detail;

import uk.co.fivium.formlibrary.input.DecimalInput;
import uk.co.fivium.formlibrary.input.StringInput;
import uk.co.nstauthority.scap.application.RemunerationModel;

public class ScapPlannedTenderDetailForm {

  private final StringInput scopeDescription;

  private final DecimalInput estimatedValue;

  private RemunerationModel remunerationModel;

  private final StringInput remunerationModelName;

  private final StringInput awardRationale;

  public ScapPlannedTenderDetailForm() {
    this.scopeDescription = new StringInput("scopeDescription", "Scope description");
    this.estimatedValue = new DecimalInput("estimatedValue", "Estimated value");
    this.remunerationModelName = new StringInput("remunerationModelName", "Remuneration model");
    this.awardRationale = new StringInput("awardRationale", "Award rationale");
  }

  public StringInput getScopeDescription() {
    return scopeDescription;
  }

  public void setScopeDescription(String scopeDescription) {
    this.scopeDescription.setInputValue(scopeDescription);
  }

  public DecimalInput getEstimatedValue() {
    return estimatedValue;
  }

  public void setEstimatedValue(String estimatedValue) {
    this.estimatedValue.setInputValue(estimatedValue);
  }

  public RemunerationModel getRemunerationModel() {
    return remunerationModel;
  }

  public void setRemunerationModel(RemunerationModel remunerationModel) {
    this.remunerationModel = remunerationModel;
  }

  public StringInput getRemunerationModelName() {
    return remunerationModelName;
  }

  public void setRemunerationModelName(String remunerationModelName) {
    this.remunerationModelName.setInputValue(remunerationModelName);
  }

  public StringInput getAwardRationale() {
    return awardRationale;
  }

  public void setAwardRationale(String awardRationale) {
    this.awardRationale.setInputValue(awardRationale);
  }
}
