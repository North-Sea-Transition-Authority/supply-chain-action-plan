package uk.co.nstauthority.scap.scap.actualtender.activity;

import jakarta.validation.constraints.NotNull;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.Errors;
import org.springframework.validation.SmartValidator;
import org.springframework.validation.ValidationUtils;
import uk.co.fivium.formlibrary.validator.string.StringInputValidator;
import uk.co.nstauthority.scap.fds.searchselector.ManualEntryUtil;
import uk.co.nstauthority.scap.scap.RemunerationModel;
import uk.co.nstauthority.scap.scap.actualtender.ActualTender;
import uk.co.nstauthority.scap.util.ValidationUtil;

@Service
class ActualTenderActivityFormValidator implements SmartValidator {

  public static final Integer MAX_SCOPE_TITLE_LENGTH = 40;
  private static final String MISSING_REMUNERATION_MODEL_ERROR =  "Select a remuneration model";
  private static final String MISSING_CONTRACT_STAGE_ERROR = "Select the stage the contract is at";
  static final String ITT_PARTICIPANTS_SELECTOR_NAME = "ittParticipantsSelector";
  private static final String MISSING_ITT_PARTICIPANTS_ERROR = "Enter at least one invitation to tender participant";
  private static final String INVALID_ITT_PARTICIPANTS_ERROR = "Enter valid invitation to tender participants only";

  private final ActualTenderActivityService actualTenderActivityService;

  @Autowired
  ActualTenderActivityFormValidator(ActualTenderActivityService actualTenderActivityService) {
    this.actualTenderActivityService = actualTenderActivityService;
  }

  @Override
  public boolean supports(@NotNull Class<?> clazz) {
    return ActualTenderActivityForm.class.equals(clazz);
  }

  @Override
  public void validate(@NotNull Object target, @NotNull Errors errors) {
    throw new IllegalArgumentException("Missing 3rd parameter of type %s"
        .formatted(ActualTenderFormValidatorHint.class));
  }

  @Override
  public void validate(@NotNull Object target, @NotNull Errors errors, @NotNull Object... validationHints) {
    var actualTenderFormValidatorHint = Arrays.stream(validationHints)
        .filter(Objects::nonNull)
        .filter(validationHint -> ActualTenderFormValidatorHint.class.equals(validationHint.getClass()))
        .map(ActualTenderFormValidatorHint.class::cast)
        .findFirst()
        .orElseThrow(() -> new IllegalStateException("Cannot get %s"
            .formatted(ActualTenderFormValidatorHint.class)));
    var actualTenderActivityFormValidatorHint = Arrays.stream(validationHints)
        .filter(Objects::nonNull)
        .filter(validationHint -> ActualTenderActivityFormValidatorHint.class.equals(validationHint.getClass()))
        .map(ActualTenderActivityFormValidatorHint.class::cast)
        .findFirst();
    var actualTender = actualTenderFormValidatorHint.actualTender();

    var form = (ActualTenderActivityForm) target;

    validateScopeTitle(form, errors, actualTender, actualTenderActivityFormValidatorHint);

    StringInputValidator.builder()
        .mustHaveCharacterCountAtMost(ValidationUtil.TEXT_AREA_STANDARD_LIMIT)
        .validate(form.getScopeDescription(), errors);
    ValidationUtils.rejectIfEmpty(
        errors, "remunerationModel",
        "remunerationModel.required",
        MISSING_REMUNERATION_MODEL_ERROR);
    if (RemunerationModel.OTHER.equals(form.getRemunerationModel())) {
      StringInputValidator.builder()
          .mustHaveCharacterCountAtMost(ValidationUtil.TEXT_AREA_STANDARD_LIMIT)
          .validate(form.getRemunerationModelName(), errors);
    }
    ValidationUtils.rejectIfEmpty(
        errors, "contractStage",
        "contractStage.required",
        MISSING_CONTRACT_STAGE_ERROR);

    validateInvitationToTenderParticipants(form.getInvitationToTenderParticipants(), errors);
  }

  private void validateScopeTitle(ActualTenderActivityForm form, Errors errors, ActualTender actualTender,
                                  Optional<ActualTenderActivityFormValidatorHint> actualTenderActivityFormValidatorHint) {
    StringInputValidator.builder()
        .mustHaveCharacterCountAtMost(MAX_SCOPE_TITLE_LENGTH)
        .validate(form.getScopeTitle(), errors);

    if (!errors.hasFieldErrors("scopeTitle.inputValue")) {

      var inputScopeTitle = form.getScopeTitle().getInputValue();
      var actualTenderActivitiesWithoutCurrent = actualTenderActivityService.getAllByActualTender(actualTender)
          .stream()
          .filter(actualTenderActivity -> actualTenderActivityFormValidatorHint
              .map(tenderActivityFormValidatorHint -> !actualTenderActivity.getId()
                  .equals(tenderActivityFormValidatorHint.currentActivityId()))
              .orElse(true));
      var existingScopeTitles = actualTenderActivitiesWithoutCurrent
          .map(ActualTenderActivity::getScopeTitle)
          .map(scopeTitle -> scopeTitle.trim().replaceAll(" +", " "))
          .map(String::toLowerCase)
          .collect(Collectors.toSet());
      var cleanedInputScopeTitle = inputScopeTitle.toLowerCase().trim().replaceAll(" +", " ");
      if (existingScopeTitles.contains(cleanedInputScopeTitle)) {
        errors.rejectValue(
            "scopeTitle.inputValue",
            "scopeTitle.notUnique",
            "There is already an actual tender activity with the scope title \"%s\"".formatted(inputScopeTitle)
        );
      }
    }
  }

  private void validateInvitationToTenderParticipants(List<String> ittParticipants, Errors errors) {
    if (Objects.isNull(ittParticipants) || ittParticipants.isEmpty()) {
      errors.rejectValue(
          ITT_PARTICIPANTS_SELECTOR_NAME,
          "%s.required".formatted(ITT_PARTICIPANTS_SELECTOR_NAME),
          MISSING_ITT_PARTICIPANTS_ERROR
      );
      return;
    }

    try {
      ManualEntryUtil.partitionManualEntries(ittParticipants);
    } catch (NumberFormatException e) {
      errors.rejectValue(
          ITT_PARTICIPANTS_SELECTOR_NAME,
          "%s.invalid".formatted(ITT_PARTICIPANTS_SELECTOR_NAME),
          INVALID_ITT_PARTICIPANTS_ERROR
      );
    }
  }
}
