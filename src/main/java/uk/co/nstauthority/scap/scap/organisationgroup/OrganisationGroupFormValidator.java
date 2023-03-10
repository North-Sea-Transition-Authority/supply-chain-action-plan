package uk.co.nstauthority.scap.scap.organisationgroup;

import org.jetbrains.annotations.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.Errors;
import org.springframework.validation.ValidationUtils;
import org.springframework.validation.Validator;
import uk.co.nstauthority.scap.authentication.UserDetailService;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;

@Service
public class OrganisationGroupFormValidator implements Validator {

  private final OrganisationGroupService organisationGroupService;

  private final UserDetailService userDetailService;

  private final TeamService teamService;

  private static final String ORGANISATION_GROUP_ID_FIELD_NAME = "organisationGroupId";
  private static final String ORGANISATION_GROUP_REQUEST_PURPOSE = "Check organisation group exists when saving scap overview";

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

    validateOrganisationGroup(form, errors);
  }

  private void validateOrganisationGroup(OrganisationGroupForm form, Errors errors) {
    var fieldName = ORGANISATION_GROUP_ID_FIELD_NAME;

    ValidationUtils.rejectIfEmpty(
        errors,
        fieldName,
        "%s.required".formatted(fieldName),
        "Select the operator for this SCAP"
    );

    if (errors.hasFieldErrors(fieldName)) {
      return;
    }

    if (organisationGroupService
        .getOrganisationGroupById(form.getOrganisationGroupId(), ORGANISATION_GROUP_REQUEST_PURPOSE).isEmpty()) {
      errors.rejectValue(
          fieldName,
          "%s.doesNotExist".formatted(fieldName),
          "Select a valid operator");
    }

    if (errors.hasFieldErrors(fieldName)) {
      return;
    }

    if (!teamService
        .userIsMemberOfOrganisationGroupTeam(form.getOrganisationGroupId(), userDetailService.getUserDetail())) {
      errors.rejectValue(
          fieldName,
          "%s.invalidTeamAuthentication".formatted(fieldName),
          "Select an operator you are allowed to create a SCAP for"
      );
    }
  }
}
