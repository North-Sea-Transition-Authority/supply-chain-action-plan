package uk.co.nstauthority.scap.permissionmanagement.regulator;

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
class RegulatorTeamMemberRolesValidatorTest {

  @InjectMocks
  RegulatorTeamMemberRolesValidator regulatorTeamMemberRolesValidator;

  @Test
  void supports_whenSupportedObject_thenTrue() {
    assertTrue(regulatorTeamMemberRolesValidator.supports(TeamMemberRolesForm.class));
  }

  @Test
  void supports_whenNonSupportedObject_thenFalse() {
    assertFalse(regulatorTeamMemberRolesValidator.supports(ValidatorTestingUtil.NonSupportedClass.class));
  }

  @Test
  void validate_allSelectedRolesExist_ValidatesSuccessfully() {
    var form = new TeamMemberRolesForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");

    form.setRoles(Set.of(RegulatorTeamRole.SCAP_VIEWER.name(),
        RegulatorTeamRole.SCAP_CASE_OFFICER.name()));

    regulatorTeamMemberRolesValidator.validate(form, bindingResult);
    assertThat(bindingResult.getAllErrors()).isEmpty();
  }

  @Test
  void validate_allSelectedRolesExist_ValidationFails() {
    var form = new TeamMemberRolesForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");

    form.setRoles(Set.of(RegulatorTeamRole.SCAP_VIEWER.name(),
        RegulatorTeamRole.SCAP_CASE_OFFICER.name(),
        "THIS IS NOT A VALID INDUSTRY ROLE"));

    regulatorTeamMemberRolesValidator.validate(form, bindingResult);
    assertThat(bindingResult.getAllErrors()).hasSize(1);
  }

  @Test
  void validate_formEmpty_ValidationFails() {
    var form = new TeamMemberRolesForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");

    regulatorTeamMemberRolesValidator.validate(form, bindingResult);
    assertThat(bindingResult.getAllErrors()).hasSize(1);
  }
}
