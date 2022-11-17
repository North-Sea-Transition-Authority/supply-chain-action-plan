package uk.co.nstauthority.scap.scap.actualtender.activity;

import java.util.Arrays;
import java.util.Objects;
import java.util.stream.Collectors;
import org.jetbrains.annotations.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.Errors;
import org.springframework.validation.SmartValidator;
import org.springframework.validation.ValidationUtils;
import uk.co.fivium.formlibrary.validator.string.StringInputValidator;
import uk.co.nstauthority.scap.scap.RemunerationModel;

@Service
class ActualTenderActivityFormValidator implements SmartValidator {

  public static final Integer MAX_SCOPE_TITLE_LENGTH = 40;
  private static final String MISSING_REMUNERATION_MODEL_ERROR =  "Select a remuneration model";
  private static final String MISSING_CONTRACT_STAGE_ERROR = "Select the stage the contract is at";

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
        .formatted(ActualTenderActivityFormValidatorHint.class));
  }

  @Override
  public void validate(@NotNull Object target, @NotNull Errors errors, @NotNull Object... validationHints) {
    var actualTenderActivityFormValidatorHint = Arrays.stream(validationHints)
        .filter(Objects::nonNull)
        .filter(validationHint -> ActualTenderActivityFormValidatorHint.class.equals(validationHint.getClass()))
        .map(ActualTenderActivityFormValidatorHint.class::cast)
        .findFirst()
        .orElseThrow(() -> new IllegalStateException("Cannot get %s"
            .formatted(ActualTenderActivityFormValidatorHint.class)));
    var actualTender = actualTenderActivityFormValidatorHint.actualTender();

    var form = (ActualTenderActivityForm) target;

    StringInputValidator.builder()
        .mustHaveCharacterCountAtMost(MAX_SCOPE_TITLE_LENGTH)
        .validate(form.getScopeTitle(), errors);
    if (!errors.hasFieldErrors("scopeTitle.inputValue")) {
      var existingScopeTitles = actualTenderActivityService.getAllByActualTender(actualTender).stream()
          .map(ActualTenderActivity::getScopeTitle)
          .map(String::toLowerCase)
          .collect(Collectors.toSet());
      if (existingScopeTitles.contains(form.getScopeTitle().getInputValue())) {
        errors.rejectValue(
            "scopeTitle.inputValue",
            "scopeTitle.notUnique",
            "There is already an actual tendering activity with the scope title \"%s\""
                .formatted(form.getScopeTitle().getInputValue().toLowerCase())
        );
      }
    }

    StringInputValidator.builder().validate(form.getScopeDescription(), errors);
    ValidationUtils.rejectIfEmpty(
        errors, "remunerationModel",
        "remunerationModel.required",
        MISSING_REMUNERATION_MODEL_ERROR);
    if (RemunerationModel.OTHER.equals(form.getRemunerationModel())) {
      StringInputValidator.builder().validate(form.getRemunerationModelName(), errors);
    }
    ValidationUtils.rejectIfEmpty(
        errors, "contractStage",
        "contractStage.required",
        MISSING_CONTRACT_STAGE_ERROR);
    StringInputValidator.builder().validate(form.getInvitationToTenderParticipants(), errors);
  }
}
