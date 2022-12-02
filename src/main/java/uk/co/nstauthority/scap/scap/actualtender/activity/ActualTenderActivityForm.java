package uk.co.nstauthority.scap.scap.actualtender.activity;

import uk.co.fivium.formlibrary.input.StringInput;
import uk.co.nstauthority.scap.scap.RemunerationModel;

public class ActualTenderActivityForm {

  private final StringInput scopeTitle;
  private final StringInput scopeDescription;
  private RemunerationModel remunerationModel;
  private final StringInput remunerationModelName;
  private ContractStage contractStage;
  private final StringInput invitationToTenderParticipants;

  public ActualTenderActivityForm() {
    this.invitationToTenderParticipants = new StringInput("invitationToTenderParticipants", "Invitation to tender participants");
    this.scopeTitle = new StringInput("scopeTitle", "Scope title");
    this.scopeDescription = new StringInput("scopeDescription", "Scope description");
    this.remunerationModelName = new StringInput("remunerationModelName", "Remuneration model name");
  }

  public StringInput getScopeTitle() {
    return scopeTitle;
  }

  public void setScopeTitle(String scopeTitle) {
    this.scopeTitle.setInputValue(scopeTitle);
  }

  public StringInput getScopeDescription() {
    return scopeDescription;
  }

  public void setScopeDescription(String scopeDescription) {
    this.scopeDescription.setInputValue(scopeDescription);
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

  public ContractStage getContractStage() {
    return contractStage;
  }

  public void setContractStage(ContractStage contractStage) {
    this.contractStage = contractStage;
  }

  public StringInput getInvitationToTenderParticipants() {
    return invitationToTenderParticipants;
  }

  public void setInvitationToTenderParticipants(String invitationToTenderParticipants) {
    this.invitationToTenderParticipants.setInputValue(invitationToTenderParticipants);
  }
}
