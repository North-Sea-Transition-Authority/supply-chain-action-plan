package uk.co.nstauthority.scap.scap.actualtender.activity.bidparticipants;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.assertj.core.api.Assertions.entry;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Collections;
import java.util.List;
import java.util.Set;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.validation.BeanPropertyBindingResult;
import uk.co.nstauthority.scap.scap.actualtender.activity.InvitationToTenderParticipant;
import uk.co.nstauthority.scap.utils.ValidatorTestingUtil;

@ExtendWith(MockitoExtension.class)
class BidParticipantsFormValidatorTest {

  @InjectMocks
  BidParticipantsFormValidator validator;

  @Test
  void supports_BidParticipantsForm_AssertTrue() {
    assertTrue(validator.supports(BidParticipantsForm.class));
  }

  @Test
  void supports_NonSupportedClass_AssertFalse() {
    assertFalse(validator.supports(ValidatorTestingUtil.NonSupportedClass.class));
  }

  @Test
  void validate_ValidForm_AssertNoErrors() {
    var participantId1 = 11;
    var participantId2 = 32;
    var participantId3 = 54;
    var form = new BidParticipantsForm();
    form.setSelectedBidParticipantIds(List.of(participantId1, participantId2, participantId3));
    var errors = new BeanPropertyBindingResult(form, "form");
    var invitationToTenderParticipants = List.of(
        new InvitationToTenderParticipant(participantId1),
        new InvitationToTenderParticipant(participantId2),
        new InvitationToTenderParticipant(participantId3)
    );

    validator.validate(form, errors, new BidParticipantsFormValidatorHint(invitationToTenderParticipants));

    assertFalse(errors.hasErrors());
  }

  @Test
  void validate_NonExistentParticipantSelected_AssertError() {
    var participantId1 = 11;
    var participantId2 = 32;
    var participantId3 = 54;
    var form = new BidParticipantsForm();
    form.setSelectedBidParticipantIds(List.of(participantId1, participantId2, participantId3));
    var errors = new BeanPropertyBindingResult(form, "form");

    validator.validate(form, errors, new BidParticipantsFormValidatorHint(Collections.emptyList()));
    var extractedErrors = ValidatorTestingUtil.extractErrors(errors);

    assertThat(extractedErrors).containsExactly(
        entry("selectedBidParticipantIds", Set.of("selectedBidParticipantIds.nonExistentIttParticipant"))
    );
  }

  @Test
  void validate_NoBidParticipantsSelected() {
    var form = new BidParticipantsForm();
    var errors = new BeanPropertyBindingResult(form, "form");

    validator.validate(form, errors, new BidParticipantsFormValidatorHint(Collections.emptyList()));
    var extractedErrors = ValidatorTestingUtil.extractErrors(errors);

    assertThat(extractedErrors).containsExactly(
        entry("selectedBidParticipantIds", Set.of("selectedBidParticipantIds.required"))
    );
  }

  @Test
  void validate_WithoutHint_AssertThrows() {
    var form = new BidParticipantsForm();
    var errors = new BeanPropertyBindingResult(form, "form");

    assertThatThrownBy(() -> validator.validate(form, errors)).isInstanceOf(IllegalStateException.class);
  }
}
