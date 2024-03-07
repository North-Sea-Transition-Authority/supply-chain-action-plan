package uk.co.nstauthority.scap.permissionmanagement.industry;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Set;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.validation.BeanPropertyBindingResult;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberRolesForm;
import uk.co.nstauthority.scap.utils.ValidatorTestingUtil;

@ExtendWith(MockitoExtension.class)
class IndustryTeamMemberRolesValidatorTest {

  @InjectMocks
  IndustryTeamMemberRolesValidator industryTeamMemberRolesValidator;

  @Test
  void supports_whenSupportedObject_thenTrue() {
    assertTrue(industryTeamMemberRolesValidator.supports(TeamMemberRolesForm.class));
  }

  @Test
  void supports_whenNonSupportedObject_thenFalse() {
    assertFalse(industryTeamMemberRolesValidator.supports(ValidatorTestingUtil.NonSupportedClass.class));
  }

  @Test
  void validate_allSelectedRolesExist_ValidatesSuccessfully() {
    var form = new TeamMemberRolesForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");

    form.setRoles(Set.of(IndustryTeamRole.SCAP_SUBMITTER.name(),
        IndustryTeamRole.SCAP_VIEWER.name()));

    industryTeamMemberRolesValidator.validate(form, bindingResult);
    assertThat(bindingResult.getAllErrors()).isEmpty();
  }

  @Test
  void validate_allSelectedRolesExist_ValidationFails() {
    var form = new TeamMemberRolesForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");

    form.setRoles(Set.of(IndustryTeamRole.SCAP_SUBMITTER.name(),
        IndustryTeamRole.SCAP_VIEWER.name(),
        "THIS IS NOT A VALID INDUSTRY ROLE"));

    industryTeamMemberRolesValidator.validate(form, bindingResult);
    assertThat(bindingResult.getAllErrors()).hasSize(1);
  }

  @Test
  void validate_formEmpty_ValidationFails() {
    var form = new TeamMemberRolesForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");

    industryTeamMemberRolesValidator.validate(form, bindingResult);
    assertThat(bindingResult.getAllErrors()).hasSize(1);
  }
}
