package uk.co.nstauthority.scap.scap.organisationgroup;

import static java.util.Map.entry;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.Optional;
import java.util.Set;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.validation.BeanPropertyBindingResult;
import uk.co.fivium.energyportalapi.generated.types.OrganisationGroup;
import uk.co.nstauthority.scap.authentication.ServiceUserDetail;
import uk.co.nstauthority.scap.authentication.ServiceUserDetailTestUtil;
import uk.co.nstauthority.scap.authentication.UserDetailService;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.scap.ScapService;
import uk.co.nstauthority.scap.utils.ValidatorTestingUtil;

@ExtendWith(MockitoExtension.class)
class OrganisationGroupFormValidatorTest {

  @Mock
  OrganisationGroupService organisationGroupService;

  @Mock
  UserDetailService userDetailService;

  @Mock
  TeamService teamService;

  @Mock
  ScapService scapService;

  @InjectMocks
  OrganisationGroupFormValidator organisationGroupFormValidator;

  private static final Integer ORG_GROUP_ID = 55;
  private static final OrganisationGroup ORGANISATION_GROUP = OrganisationGroup.newBuilder()
      .organisationGroupId(ORG_GROUP_ID)
      .build();
  private static final ServiceUserDetail USER = ServiceUserDetailTestUtil.Builder().build();

  @Test
  void supports_organisationGroupForm_assertTrue() {
    var supportedClass = OrganisationGroupForm.class;

    assertTrue(organisationGroupFormValidator.supports(supportedClass));
  }

  @Test
  void supports_nonSupportedClass_assertFalse() {
    var unsupportedClass = ValidatorTestingUtil.NonSupportedClass.class;

    assertFalse(organisationGroupFormValidator.supports(unsupportedClass));
  }

  @Test
  void validate_validForm() {
    var form = getValidForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");
    var requestPurpose = "Check organisation group exists when saving scap overview";

    when(organisationGroupService.getOrganisationGroupById(ORG_GROUP_ID, requestPurpose))
        .thenReturn(Optional.of(ORGANISATION_GROUP));
    when(userDetailService.getUserDetail()).thenReturn(USER);
    when(teamService.userIsMemberOfOrganisationGroupTeam(any(), eq(USER))).thenReturn(true);

    organisationGroupFormValidator.validate(form, bindingResult);

    assertFalse(bindingResult.hasErrors());
  }

  @Test
  void validate_EmptyForm() {
    var form = new OrganisationGroupForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");
    organisationGroupFormValidator.validate(form, bindingResult);
    var extractedErrors = ValidatorTestingUtil.extractErrors(bindingResult);

    assertThat(extractedErrors).containsExactly(
        entry("organisationGroupId", Set.of("required")),
        entry("isTierOneContractor", Set.of("required"))
    );
  }

  @Test
  void validate_nonExistentOrganisationGroup() {
    var form = getValidForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");
    organisationGroupFormValidator.validate(form, bindingResult);
    var extractedErrors = ValidatorTestingUtil.extractErrors(bindingResult);

    assertThat(extractedErrors).containsExactly(
        entry("organisationGroupId", Set.of("doesNotExist")));
  }

  @Test
  void validate_userNotPartOfOrganisationGroup() {
    var form = getValidForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");
    var requestPurpose = "Check organisation group exists when saving scap overview";

    when(organisationGroupService.getOrganisationGroupById(ORG_GROUP_ID, requestPurpose))
        .thenReturn(Optional.of(ORGANISATION_GROUP));
    when(userDetailService.getUserDetail()).thenReturn(USER);
    when(teamService.userIsMemberOfOrganisationGroupTeam(any(), eq(USER))).thenReturn(false);

    organisationGroupFormValidator.validate(form, bindingResult);

    var extractedErrors = ValidatorTestingUtil.extractErrors(bindingResult);
    assertThat(extractedErrors).containsExactly(
        entry("organisationGroupId", Set.of("organisationGroupId.invalidTeamAuthentication")));
  }

  @Test
  void validate_IsTierOneContractor_NoScapSelected() {
    var form = getValidForm();
    form.setIsTierOneContractor(true);
    var bindingResult = ValidatorTestingUtil.bindingResultWithoutErrors(form);
    when(organisationGroupService.getOrganisationGroupById(
        ORG_GROUP_ID, OrganisationGroupFormValidator.ORGANISATION_GROUP_REQUEST_PURPOSE
    )).thenReturn(Optional.of(ORGANISATION_GROUP));
    when(userDetailService.getUserDetail()).thenReturn(USER);
    when(teamService.userIsMemberOfOrganisationGroupTeam(any(), eq(USER))).thenReturn(true);

    organisationGroupFormValidator.validate(form, bindingResult);

    var extractedErrors = ValidatorTestingUtil.extractErrors(bindingResult);

    assertThat(extractedErrors).containsExactly(
        entry(OrganisationGroupFormValidator.SCAP_ID_FIELD_NAME, Collections.singleton("required"))
    );
  }

  @Test
  void validate_IsTierOneContractor_InvalidScapSelected() {
    var form = getValidForm();
    form.setIsTierOneContractor(true);
    form.setParentScapId(999);
    var bindingResult = ValidatorTestingUtil.bindingResultWithoutErrors(form);
    when(organisationGroupService.getOrganisationGroupById(
        ORG_GROUP_ID, OrganisationGroupFormValidator.ORGANISATION_GROUP_REQUEST_PURPOSE
    )).thenReturn(Optional.of(ORGANISATION_GROUP));
    when(userDetailService.getUserDetail()).thenReturn(USER);
    when(teamService.userIsMemberOfOrganisationGroupTeam(any(), eq(USER))).thenReturn(true);
    when(scapService.existsById(ScapId.valueOf(form.getParentScapId()))).thenReturn(false);

    organisationGroupFormValidator.validate(form, bindingResult);

    var extractedErrors = ValidatorTestingUtil.extractErrors(bindingResult);

    assertThat(extractedErrors).containsExactly(
        entry(OrganisationGroupFormValidator.SCAP_ID_FIELD_NAME, Collections.singleton("invalid"))
    );
  }

  @Test
  void validate_IsTierOneContractor_ValidScapSelected() {
    var form = getValidForm();
    form.setIsTierOneContractor(true);
    form.setParentScapId(1);
    var bindingResult = ValidatorTestingUtil.bindingResultWithoutErrors(form);
    when(organisationGroupService.getOrganisationGroupById(
        ORG_GROUP_ID, OrganisationGroupFormValidator.ORGANISATION_GROUP_REQUEST_PURPOSE
    )).thenReturn(Optional.of(ORGANISATION_GROUP));
    when(userDetailService.getUserDetail()).thenReturn(USER);
    when(teamService.userIsMemberOfOrganisationGroupTeam(any(), eq(USER))).thenReturn(true);
    when(scapService.existsById(ScapId.valueOf(form.getParentScapId()))).thenReturn(true);

    organisationGroupFormValidator.validate(form, bindingResult);

    assertFalse(bindingResult.hasErrors());
  }
  
  private OrganisationGroupForm getValidForm() {
    var form = new OrganisationGroupForm();
    form.setOrganisationGroupId(ORG_GROUP_ID);
    form.setIsTierOneContractor(false);
    return form;
  }
}
