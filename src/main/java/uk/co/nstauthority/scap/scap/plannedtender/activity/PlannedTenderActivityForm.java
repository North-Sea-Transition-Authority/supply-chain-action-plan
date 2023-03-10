package uk.co.nstauthority.scap.scap.plannedtender.activity;

import java.time.LocalDate;
import java.util.Objects;
import uk.co.fivium.formlibrary.input.DecimalInput;
import uk.co.fivium.formlibrary.input.StringInput;
import uk.co.fivium.formlibrary.input.ThreeFieldDateInput;
import uk.co.nstauthority.scap.scap.RemunerationModel;

public class PlannedTenderActivityForm {

  private final StringInput scopeDescription;

  private final DecimalInput estimatedValue;

  private RemunerationModel remunerationModel;

  private final StringInput remunerationModelName;

  private final StringInput awardRationale;

  private final ThreeFieldDateInput indicativeActualTenderStartDate;

  private final ThreeFieldDateInput indicativeContractAwardDate;

  static final String INDICATIVE_ACTUAL_TENDER_START_DATE_FIELD = "indicativeActualTenderStartDate";
  static final String INDICATIVE_CONTRACT_AWARD_DATE_FIELD = "indicativeContractAwardDate";

  public PlannedTenderActivityForm() {
    this.scopeDescription = new StringInput("scopeDescription", "the scope description");
    this.estimatedValue = new DecimalInput("estimatedValue", "the estimated value");
    this.remunerationModelName = new StringInput("remunerationModelName", "the remuneration model");
    this.awardRationale = new StringInput("awardRationale", "the award rationale");
    this.indicativeActualTenderStartDate = new ThreeFieldDateInput(
        INDICATIVE_ACTUAL_TENDER_START_DATE_FIELD, "indicative actual tender start date"
    );
    this.indicativeContractAwardDate = new ThreeFieldDateInput(
        INDICATIVE_CONTRACT_AWARD_DATE_FIELD, "indicative contract award date"
    );
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

  public ThreeFieldDateInput getIndicativeActualTenderStartDate() {
    return indicativeActualTenderStartDate;
  }

  public void setIndicativeActualTenderStartDate(LocalDate localDate) {
    if (Objects.nonNull(localDate)) {
      indicativeActualTenderStartDate.setDate(localDate);
    }
  }

  public ThreeFieldDateInput getIndicativeContractAwardDate() {
    return indicativeContractAwardDate;
  }

  public void setIndicativeContractAwardDate(LocalDate localDate) {
    if (Objects.nonNull(localDate)) {
      indicativeContractAwardDate.setDate(localDate);
    }
  }
}
