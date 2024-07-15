package uk.co.nstauthority.scap.scap.actualtender.activity.bidparticipants;

import jakarta.validation.constraints.NotNull;
import java.util.Arrays;
import java.util.Objects;
import java.util.stream.Collectors;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.stereotype.Service;
import org.springframework.validation.Errors;
import org.springframework.validation.SmartValidator;
import uk.co.nstauthority.scap.scap.actualtender.activity.InvitationToTenderParticipant;

@Service
class BidParticipantsFormValidator implements SmartValidator {

  private static final String MISSING_BID_PARTICIPANTS_MESSAGE = "Select at least one bid participant";
  private static final String INVALID_BID_PARTICIPANTS_MESSAGE = "Select valid bid participants";

  @Override
  public boolean supports(@NotNull Class<?> clazz) {
    return BidParticipantsForm.class.equals(clazz);
  }

  @Override
  public void validate(@NotNull Object target, @NotNull Errors errors) {
    throw new IllegalStateException("Cannot validate without BidParticipantsFormValidatorHint");
  }

  @Override
  public void validate(@NotNull Object target, @NotNull Errors errors, @NotNull Object... validationHints) {
    var bidParticipantsFormValidatorHint = Arrays.stream(validationHints)
        .filter(Objects::nonNull)
        .filter(validationHint -> BidParticipantsFormValidatorHint.class.equals(validationHint.getClass()))
        .map(BidParticipantsFormValidatorHint.class::cast)
        .findFirst()
        .orElseThrow(() -> new IllegalStateException("Cannot get BidParticipantsFormValidatorHint"));
    var fieldName = "selectedBidParticipantIds";

    var form = (BidParticipantsForm) target;

    if (CollectionUtils.isEmpty(form.getSelectedBidParticipantIds())) {
      errors.rejectValue(
          fieldName,
          String.format("%s.required", fieldName),
          MISSING_BID_PARTICIPANTS_MESSAGE
      );
    }

    if (!errors.hasFieldErrors(fieldName)) {
      var permittedParticipantIds = bidParticipantsFormValidatorHint.invitationToTenderParticipants().stream()
          .map(InvitationToTenderParticipant::getId)
          .collect(Collectors.toSet());
      if (!permittedParticipantIds.containsAll(form.getSelectedBidParticipantIds())) {
        errors.rejectValue(
            fieldName,
            String.format("%s.nonExistentIttParticipant", fieldName),
            INVALID_BID_PARTICIPANTS_MESSAGE
        );
      }
    }
  }
}
