package uk.co.nstauthority.scap.permissionmanagement.teams;

import jakarta.validation.constraints.NotNull;
import java.math.BigDecimal;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;
import uk.co.fivium.formlibrary.validator.decimal.DecimalInputValidator;
import uk.co.nstauthority.scap.scap.organisationgroup.OrganisationGroupService;

@Service
public class NewTeamFormValidator implements Validator {

  private final OrganisationGroupService organisationGroupService;

  private final TeamService teamService;

  @Autowired
  public NewTeamFormValidator(OrganisationGroupService organisationGroupService, TeamService teamService) {
    this.organisationGroupService = organisationGroupService;
    this.teamService = teamService;
  }

  @Override
  public boolean supports(@NotNull Class<?> clazz) {
    return NewTeamForm.class.equals(clazz);
  }

  @Override
  public void validate(@NotNull Object target, @NotNull Errors errors) {
    var form = (NewTeamForm) target;
    var purpose = "Check organisation group exists when creating new SCAP team";

    DecimalInputValidator.builder()
        .mustBeMoreThanOrEqualTo(BigDecimal.valueOf(0))
        .mustBeLessThanOrEqualTo(BigDecimal.valueOf(2147483647))
        .mustHaveNoMoreThanDecimalPlaces(0)
        .validate(form.getOrganisationGroupId(), errors);
    if (errors.hasFieldErrors("organisationGroupId.inputValue")) {
      return;
    }
    var organisationGroupId = Integer.parseInt(form.getOrganisationGroupId().getInputValue());
    if (teamService.findByEnergyPortalOrgGroupId(organisationGroupId).isPresent()) {
      errors.rejectValue("organisationGroupId.inputValue",
          "organisationGroupId.alreadyExists",
          "This organisation group already has a team on SCAP");
    }
    if (errors.hasFieldErrors("organisationGroupId.inputValue")) {
      return;
    }
    if (organisationGroupService.getOrganisationGroupById(organisationGroupId, purpose).isEmpty()) {
      errors.rejectValue(
          "organisationGroupId.inputValue",
          "organisationGroupId.doesNotExist",
          "The selected organisation group does not exist");
    }
  }
}
