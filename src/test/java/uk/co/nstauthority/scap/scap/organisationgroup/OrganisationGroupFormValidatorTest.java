package uk.co.nstauthority.scap.scap.organisationgroup;

import static java.util.Map.entry;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

import java.util.Optional;
import java.util.Set;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.validation.BeanPropertyBindingResult;
import uk.co.fivium.energyportalapi.generated.types.OrganisationGroup;
import uk.co.nstauthority.scap.utils.ValidatorTestingUtil;

@ExtendWith(MockitoExtension.class)
class OrganisationGroupFormValidatorTest {

  @Mock
  OrganisationGroupService organisationGroupService;

  @InjectMocks
  OrganisationGroupFormValidator organisationGroupFormValidator;

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
    var form = new OrganisationGroupForm();
    form.setOrganisationGroupId("1");
    var bindingResult = new BeanPropertyBindingResult(form, "form");
    var requestPurpose = "Check organisation group exists when saving scap overview";
    var organisationGroup = new OrganisationGroup(1, null, null, null, null, null);

    when(organisationGroupService.getOrganisationGroupById(organisationGroup.getOrganisationGroupId(), requestPurpose))
        .thenReturn(Optional.of(organisationGroup));
    organisationGroupFormValidator.validate(form, bindingResult);

    assertFalse(bindingResult.hasErrors());
  }

  @Test
  void validate_nullOrganisationGroup() {
    var form = new OrganisationGroupForm();
    form.setOrganisationGroupId(null);
    var bindingResult = new BeanPropertyBindingResult(form, "form");
    organisationGroupFormValidator.validate(form, bindingResult);
    var extractedErrors = ValidatorTestingUtil.extractErrors(bindingResult);

    assertThat(extractedErrors).containsExactly(
        entry("organisationGroupId.inputValue", Set.of("organisationGroupId.required")));
  }

  @Test
  void validate_nonExistentOrganisationGroup() {
    var form = new OrganisationGroupForm();
    form.setOrganisationGroupId("999");
    var bindingResult = new BeanPropertyBindingResult(form, "form");
    organisationGroupFormValidator.validate(form, bindingResult);
    var extractedErrors = ValidatorTestingUtil.extractErrors(bindingResult);

    assertThat(extractedErrors).containsExactly(
        entry("organisationGroupId.inputValue", Set.of("organisationGroupId.doesNotExist")));
  }

}
