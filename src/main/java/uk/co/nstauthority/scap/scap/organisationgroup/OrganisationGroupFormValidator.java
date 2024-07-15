package uk.co.nstauthority.scap.scap.organisationgroup;

import jakarta.validation.constraints.NotNull;
import java.util.Objects;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.Errors;
import org.springframework.validation.ValidationUtils;
import org.springframework.validation.Validator;
import uk.co.nstauthority.scap.authentication.UserDetailService;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.scap.ScapService;
import uk.co.nstauthority.scap.util.ValidatorErrorCodes;

@Service
public class OrganisationGroupFormValidator implements Validator {

  private final OrganisationGroupService organisationGroupService;

  private final UserDetailService userDetailService;

  private final TeamService teamService;
  private final ScapService scapService;

  static final String ORGANISATION_GROUP_ID_FIELD_NAME = "organisationGroupId";
  static final String ORGANISATION_GROUP_REQUEST_PURPOSE = "Check organisation group exists when saving scap overview";
  static final String SCAP_ID_FIELD_NAME = "parentScapId";

  @Autowired
  public OrganisationGroupFormValidator(OrganisationGroupService organisationGroupService,
                                        UserDetailService userDetailService,
                                        TeamService teamService,
                                        ScapService scapService) {
    this.organisationGroupService = organisationGroupService;
    this.userDetailService = userDetailService;
    this.teamService = teamService;
    this.scapService = scapService;
  }

  @Override
  public boolean supports(@NotNull Class<?> clazz) {
    return OrganisationGroupForm.class.equals(clazz);
  }

  @Override
  public void validate(@NotNull Object target, @NotNull Errors errors) {
    var form = (OrganisationGroupForm) target;

    validateOrganisationGroup(form, errors);
    validateTierOneContractorQuestion(form, errors);
  }

  private void validateOrganisationGroup(OrganisationGroupForm form, Errors errors) {
    var fieldName = ORGANISATION_GROUP_ID_FIELD_NAME;

    ValidationUtils.rejectIfEmpty(
        errors,
        fieldName,
        ValidatorErrorCodes.REQUIRED.getErrorCode(),
        "Select the operator for this SCAP"
    );

    if (errors.hasFieldErrors(fieldName)) {
      return;
    }

    if (organisationGroupService
        .getOrganisationGroupById(form.getOrganisationGroupId(), ORGANISATION_GROUP_REQUEST_PURPOSE).isEmpty()) {
      errors.rejectValue(
          fieldName,
          ValidatorErrorCodes.DOES_NOT_EXIST.getErrorCode(),
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

  private void validateTierOneContractorQuestion(OrganisationGroupForm form, Errors errors) {
    if (Objects.isNull(form.getIsTierOneContractor())) {
      errors.rejectValue(
          "isTierOneContractor",
          ValidatorErrorCodes.REQUIRED.getErrorCode(),
          "Select whether you are submitting this SCAP on behalf of a tier one contractor"
      );
      return;
    }

    if (Boolean.FALSE.equals(form.getIsTierOneContractor())) {
      return;
    }

    if (Objects.isNull(form.getParentScapId())) {
      errors.rejectValue(
          SCAP_ID_FIELD_NAME,
          ValidatorErrorCodes.REQUIRED.getErrorCode(),
          "Enter the reference of the parent SCAP"
      );
      return;
    }

    if (!scapService.existsById(ScapId.valueOf(form.getParentScapId()))) {
      errors.rejectValue(
          SCAP_ID_FIELD_NAME,
          ValidatorErrorCodes.INVALID.getErrorCode(),
          "Enter a valid SCAP reference"
      );
    }
  }
}
