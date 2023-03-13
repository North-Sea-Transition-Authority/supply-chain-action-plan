package uk.co.nstauthority.scap.scap.actualtender.activity;

import java.util.List;
import uk.co.fivium.formlibrary.input.StringInput;
import uk.co.nstauthority.scap.scap.RemunerationModel;

public class ActualTenderActivityForm {

  private final StringInput scopeTitle;
  private final StringInput scopeDescription;
  private RemunerationModel remunerationModel;
  private final StringInput remunerationModelName;
  private List<String> invitationToTenderParticipants;
  private Object ittParticipantsSelector;
  private ContractStage contractStage;

  public ActualTenderActivityForm() {
    this.scopeTitle = new StringInput("scopeTitle", "a scope title");
    this.scopeDescription = new StringInput("scopeDescription", "the scope description");
    this.remunerationModelName = new StringInput("remunerationModelName", "the remuneration model");
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

  public List<String> getInvitationToTenderParticipants() {
    return invitationToTenderParticipants;
  }

  public void setInvitationToTenderParticipants(List<String> invitationToTenderParticipants) {
    this.invitationToTenderParticipants = invitationToTenderParticipants;
  }

  public Object getIttParticipantsSelector() {
    return ittParticipantsSelector;
  }

  public void setIttParticipantsSelector(Object ittParticipantsSelector) {
    this.ittParticipantsSelector = ittParticipantsSelector;
  }

  public ContractStage getContractStage() {
    return contractStage;
  }

  public void setContractStage(ContractStage contractStage) {
    this.contractStage = contractStage;
  }
}
