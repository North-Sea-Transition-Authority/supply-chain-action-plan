package uk.co.nstauthority.scap.permissionmanagement.teams;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Service;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;
import uk.co.fivium.formlibrary.validator.string.StringInputValidator;
import uk.co.nstauthority.scap.energyportal.EnergyPortalUserService;
import uk.co.nstauthority.scap.permissionmanagement.AddTeamMemberForm;

@Service
public class AddTeamMemberValidator implements Validator {

  private static final String EMAIL_FORM_FIELD_NAME = "email.inputValue";

  private static final String EMAIL_NOT_FOUND_ERROR_CODE = "%s.notFound".formatted(EMAIL_FORM_FIELD_NAME);
  private static final String EMAIL_NOT_FOUND_ERROR_MESSAGE = "No UK Energy Portal user exists with this email";

  private final EnergyPortalUserService energyPortalUserService;

  @Autowired
  AddTeamMemberValidator(EnergyPortalUserService energyPortalUserService) {
    this.energyPortalUserService = energyPortalUserService;
  }

  @Override
  public boolean supports(@NonNull Class<?> clazz) {
    return AddTeamMemberForm.class.equals(clazz);
  }

  @Override
  public void validate(@NonNull Object target, @NonNull Errors errors) {

    var form = (AddTeamMemberForm) target;

    StringInputValidator
        .builder()
        .validate(form.getEmail(), errors);


    if (errors.getFieldErrors(EMAIL_FORM_FIELD_NAME).isEmpty()) {

      var resultingUsers = energyPortalUserService.findUserByEmail(form.getEmail().getInputValue());
      if (resultingUsers.isEmpty()) {
        errors.rejectValue(
            EMAIL_FORM_FIELD_NAME,
            EMAIL_NOT_FOUND_ERROR_CODE,
            EMAIL_NOT_FOUND_ERROR_MESSAGE
        );
      }
    }
  }
}
