package uk.co.nstauthority.scap.scap.organisationgroup;

import java.math.BigDecimal;
import org.jetbrains.annotations.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;
import uk.co.fivium.formlibrary.validator.decimal.DecimalInputValidator;
import uk.co.nstauthority.scap.authentication.UserDetailService;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;

@Service
public class OrganisationGroupFormValidator implements Validator {

  private final OrganisationGroupService organisationGroupService;

  private final UserDetailService userDetailService;

  private final TeamService teamService;

  @Autowired
  public OrganisationGroupFormValidator(OrganisationGroupService organisationGroupService,
                                        UserDetailService userDetailService, TeamService teamService) {
    this.organisationGroupService = organisationGroupService;
    this.userDetailService = userDetailService;
    this.teamService = teamService;
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

    if (!errors.hasErrors() && !teamService
        .userIsMemberOfOrganisationGroupTeam(Integer.parseInt(form.getOrganisationGroupId().getInputValue()),
            userDetailService.getUserDetail())) {
      errors.rejectValue(
          "organisationGroupId.inputValue",
          "organisationGroupId.invalidTeamAuthentication",
          "You must be a member of this organisation to generate a new SCAP for this organisation"
      );
    }
  }
}
