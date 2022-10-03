package uk.co.nstauthority.scap.application.organisationgroup;

import java.math.BigDecimal;
import org.jetbrains.annotations.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;
import uk.co.fivium.formlibrary.validator.decimal.DecimalInputValidator;

@Service
public class OrganisationGroupFormValidator implements Validator {

  private final OrganisationGroupService organisationGroupService;

  @Autowired
  public OrganisationGroupFormValidator(OrganisationGroupService organisationGroupService) {
    this.organisationGroupService = organisationGroupService;
  }

  @Override
  public boolean supports(@NotNull Class<?> clazz) {
    return OrganisationGroupForm.class.equals(clazz);
  }

  @Override
  public void validate(@NotNull Object target, @NotNull Errors errors) {
    var form = (OrganisationGroupForm) target;
    var purpose = "Check organisation group exists when saving scap overview";

    DecimalInputValidator.builder()
        .mustBeMoreThanOrEqual(BigDecimal.valueOf(0))
        .mustBeLessThanOrEqualTo(BigDecimal.valueOf(2147483647))
        .mustHaveNoMoreThanDecimalPlaces(0)
        .validate(form.getOrganisationGroupId(), errors);

    if (!errors.hasErrors() && organisationGroupService
        .getOrganisationGroupById(Integer.valueOf(form.getOrganisationGroupId().getInputValue()), purpose).isEmpty()) {
      errors.rejectValue(
          "organisationGroupId.inputValue",
          "organisationGroupId.doesNotExist",
          "That operator does not exist");
    }

  }
}
