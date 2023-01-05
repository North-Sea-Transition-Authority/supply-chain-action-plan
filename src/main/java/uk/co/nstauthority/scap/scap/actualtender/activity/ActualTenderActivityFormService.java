package uk.co.nstauthority.scap.scap.actualtender.activity;

import java.util.List;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.BindingResult;
import uk.co.nstauthority.scap.scap.actualtender.ActualTender;

@Service
class ActualTenderActivityFormService {

  private final ActualTenderActivityFormValidator validator;

  @Autowired
  ActualTenderActivityFormService(ActualTenderActivityFormValidator validator) {
    this.validator = validator;
  }

  BindingResult validate(ActualTenderActivityForm form, BindingResult bindingResult, ActualTender actualTender) {
    validator.validate(form, bindingResult, new ActualTenderFormValidatorHint(actualTender));
    return bindingResult;
  }

  BindingResult validate(ActualTenderActivityForm form,
                         BindingResult bindingResult,
                         ActualTender actualTender,
                         ActualTenderActivity actualTenderActivity) {
    validator.validate(
        form,
        bindingResult,
        new ActualTenderFormValidatorHint(actualTender),
        new ActualTenderActivityFormValidatorHint(actualTenderActivity.getId()));
    return bindingResult;
  }

  ActualTenderActivityForm getForm(ActualTenderActivity actualTenderActivity,
                                   List<InvitationToTenderParticipant> invitationToTenderParticipants) {
    var form = new ActualTenderActivityForm();
    form.setScopeTitle(actualTenderActivity.getScopeTitle());
    form.setScopeDescription(actualTenderActivity.getScopeDescription());
    form.setRemunerationModel(actualTenderActivity.getRemunerationModel());
    form.setRemunerationModelName(actualTenderActivity.getRemunerationModelName());
    form.setContractStage(actualTenderActivity.getContractStage());
    var participantNames = invitationToTenderParticipants.stream()
        .map(InvitationToTenderParticipant::getCompanyName)
        .toList();
    form.setInvitationToTenderParticipants(participantNames);
    return form;
  }
}
