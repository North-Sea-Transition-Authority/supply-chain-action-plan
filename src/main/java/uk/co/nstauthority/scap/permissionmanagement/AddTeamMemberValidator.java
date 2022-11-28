package uk.co.nstauthority.scap.permissionmanagement;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Service;
import org.springframework.validation.Errors;
import org.springframework.validation.ValidationUtils;
import org.springframework.validation.Validator;
import uk.co.fivium.formlibrary.validator.string.StringInputValidator;
import uk.co.nstauthority.scap.energyportal.EnergyPortalUserService;

@Service
public class AddTeamMemberValidator implements Validator {

  private static final String USERNAME_FORM_FIELD_NAME = "username";

  private static final String NO_USERNAME_ERROR_CODE = "%s.required".formatted(USERNAME_FORM_FIELD_NAME);
  private static final String NO_USERNAME_ERROR_MESSAGE = "Enter an Energy Portal username";

  private static final String USERNAME_NOT_FOUND_ERROR_CODE = "%s.notFound".formatted(USERNAME_FORM_FIELD_NAME);
  private static final String USERNAME_NOT_FOUND_ERROR_MESSAGE = "No Energy Portal user exists with this username";

  private static final String TOO_MANY_RESULTS_FOUND_ERROR_CODE = "%s.tooManyResults".formatted(USERNAME_FORM_FIELD_NAME);
  private static final String TOO_MANY_RESULTS_FOUND_ERROR_MESSAGE =
      "More than one Energy Portal user exists with this email address. Enter the username of the user instead.";

  private static final String SHARED_ACCOUNT_NOT_ALLOWED_ERROR_CODE = "%s.sharedAccountProhibited"
      .formatted(USERNAME_FORM_FIELD_NAME);

  private static final String SHARED_ACCOUNT_NOT_ALLOWED_ERROR_MESSAGE = "You cannot add shared accounts to this service";

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

    ValidationUtils.rejectIfEmptyOrWhitespace(
        errors,
        USERNAME_FORM_FIELD_NAME,
        NO_USERNAME_ERROR_CODE,
        NO_USERNAME_ERROR_MESSAGE
    );

    StringInputValidator
        .builder()
        .validate(form.getUsername(), errors);


    if (errors.getFieldErrors("username.inputValue").isEmpty()) {

      var resultingUsers = energyPortalUserService.findUsersByUsername(form.getUsername().getInputValue());
      if (resultingUsers.isEmpty()) {
        errors.rejectValue(
            USERNAME_FORM_FIELD_NAME,
            USERNAME_NOT_FOUND_ERROR_CODE,
            USERNAME_NOT_FOUND_ERROR_MESSAGE
        );
      } else if (resultingUsers.size() > 1) {
        errors.rejectValue(
            USERNAME_FORM_FIELD_NAME,
            TOO_MANY_RESULTS_FOUND_ERROR_CODE,
            TOO_MANY_RESULTS_FOUND_ERROR_MESSAGE
        );
      } else if (resultingUsers.get(0).isSharedAccount()) {
        errors.rejectValue(
            USERNAME_FORM_FIELD_NAME,
            SHARED_ACCOUNT_NOT_ALLOWED_ERROR_CODE,
            SHARED_ACCOUNT_NOT_ALLOWED_ERROR_MESSAGE
        );
      }
    }
  }
}