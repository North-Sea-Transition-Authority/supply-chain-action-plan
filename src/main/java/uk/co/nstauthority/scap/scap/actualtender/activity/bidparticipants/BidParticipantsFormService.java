package uk.co.nstauthority.scap.scap.actualtender.activity.bidparticipants;

import java.util.List;
import java.util.Map;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.BindingResult;
import uk.co.nstauthority.scap.scap.actualtender.activity.InvitationToTenderParticipant;
import uk.co.nstauthority.scap.util.StreamUtils;

@Service
class BidParticipantsFormService {

  private final BidParticipantsFormValidator validator;

  @Autowired
  BidParticipantsFormService(BidParticipantsFormValidator validator) {
    this.validator = validator;
  }

  BindingResult validate(BidParticipantsForm form, BindingResult bindingResult,
                         List<InvitationToTenderParticipant> invitationToTenderParticipants) {
    validator.validate(form, bindingResult, new BidParticipantsFormValidatorHint(invitationToTenderParticipants));
    return bindingResult;
  }

  static Map<String, String> getBidParticipantsCheckboxes(List<InvitationToTenderParticipant> participants) {
    return participants.stream()
        .collect(StreamUtils.toLinkedHashMap(
            participant -> String.valueOf(participant.getId()),
            InvitationToTenderParticipant::getCompanyName
        ));
  }
}
