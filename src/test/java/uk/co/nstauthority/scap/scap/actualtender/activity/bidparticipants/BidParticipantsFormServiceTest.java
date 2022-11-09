package uk.co.nstauthority.scap.scap.actualtender.activity.bidparticipants;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;

import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.validation.BeanPropertyBindingResult;
import uk.co.nstauthority.scap.scap.actualtender.activity.InvitationToTenderParticipant;

@ExtendWith(MockitoExtension.class)
class BidParticipantsFormServiceTest {

  @Mock
  BidParticipantsFormValidator validator;

  @InjectMocks
  BidParticipantsFormService bidParticipantsFormService;

  @Test
  void validate_verifyCallsValidator() {
    var form = new BidParticipantsForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");
    var invitationToTenderParticipants = List.of(new InvitationToTenderParticipant(140));
    var argumentCaptor = ArgumentCaptor.forClass(BidParticipantsFormValidatorHint.class);

    var returnedBindingResult = bidParticipantsFormService.validate(form, bindingResult, invitationToTenderParticipants);

    verify(validator).validate(eq(form), eq(bindingResult), argumentCaptor.capture());
    assertThat(returnedBindingResult).isEqualTo(bindingResult);
    assertThat(argumentCaptor.getValue())
        .extracting(BidParticipantsFormValidatorHint::invitationToTenderParticipants)
        .isEqualTo(invitationToTenderParticipants);
  }
}
