package uk.co.nstauthority.scap.scap.actualtender.activity;

import org.jetbrains.annotations.NotNull;
import org.springframework.stereotype.Service;
import org.springframework.validation.Errors;
import org.springframework.validation.ValidationUtils;
import org.springframework.validation.Validator;
import uk.co.fivium.formlibrary.validator.string.StringInputValidator;
import uk.co.nstauthority.scap.scap.RemunerationModel;

@Service
class ActualTenderActivityFormValidator implements Validator {

  private static final String MISSING_REMUNERATION_MODEL_ERROR =  "Select a remuneration model";
  private static final String MISSING_CONTRACT_STAGE_ERROR = "Select the stage the contract is at";

  @Override
  public boolean supports(@NotNull Class<?> clazz) {
    return ActualTenderActivityForm.class.equals(clazz);
  }

  @Override
  public void validate(@NotNull Object target, @NotNull Errors errors) {
    var form = (ActualTenderActivityForm) target;

    StringInputValidator.builder().validate(form.getScopeTitle(), errors);
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
