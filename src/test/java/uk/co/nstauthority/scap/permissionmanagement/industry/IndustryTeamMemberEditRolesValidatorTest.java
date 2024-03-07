package uk.co.nstauthority.scap.permissionmanagement.industry;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Set;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.context.support.DefaultMessageSourceResolvable;
import org.springframework.validation.BeanPropertyBindingResult;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberRolesForm;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberTestUtil;
import uk.co.nstauthority.scap.permissionmanagement.TeamTestUtil;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberRemovalService;
import uk.co.nstauthority.scap.utils.ValidatorTestingUtil;

@ExtendWith(MockitoExtension.class)
class IndustryTeamMemberEditRolesValidatorTest {

  @Mock
  TeamMemberRemovalService teamMemberRemovalService;

  @InjectMocks
  IndustryTeamMemberEditRolesValidator industryTeamMemberEditRolesValidator;

  @Test
  void supports_whenSupportedObject_thenTrue() {
    assertTrue(industryTeamMemberEditRolesValidator.supports(TeamMemberRolesForm.class));
  }

  @Test
  void supports_whenNonSupportedObject_thenFalse() {
    assertFalse(industryTeamMemberEditRolesValidator.supports(ValidatorTestingUtil.NonSupportedClass.class));
  }

  @Test
  void validate_NoValidationHint_ThrowsError() {
    var form = new TeamMemberRolesForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");

    assertThatThrownBy( () -> industryTeamMemberEditRolesValidator.validate(form,bindingResult))
        .isInstanceOf(IllegalArgumentException.class);
  }

  @Test
  void validate_allSelectedRolesExist_ValidatesSuccessfully() {
    var form = new TeamMemberRolesForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");

    var team = TeamTestUtil.Builder().build();
    var teamMember = TeamMemberTestUtil
        .Builder()
        .withRole(IndustryTeamRole.ACCESS_MANAGER)
        .build();
    var dto = new IndustryTeamMemberEditRolesValidatorDto(team, teamMember);

    form.setRoles(Set.of(IndustryTeamRole.SCAP_SUBMITTER.name(),
        IndustryTeamRole.SCAP_VIEWER.name(),
        IndustryTeamRole.ACCESS_MANAGER.name()));


    industryTeamMemberEditRolesValidator.validate(form, bindingResult, dto);
    assertFalse(bindingResult.hasErrors());
  }

  @Test
  void validate_RoleDoesntExist_ValidationFails() {
    var form = new TeamMemberRolesForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");

    var team = TeamTestUtil.Builder().build();
    var teamMember = TeamMemberTestUtil.Builder().build();
    var dto = new IndustryTeamMemberEditRolesValidatorDto(team, teamMember);

    form.setRoles(Set.of(IndustryTeamRole.SCAP_SUBMITTER.name(),
        IndustryTeamRole.SCAP_VIEWER.name(),
        "THIS IS NOT A VALID INDUSTRY ROLE"));

    industryTeamMemberEditRolesValidator.validate(form, bindingResult, dto);
    assertThat(bindingResult.getAllErrors()).hasSize(2);
    var codes = ValidatorTestingUtil.extractErrors(bindingResult);
    assertTrue(codes.get("roles").contains("roles.notValid"));
  }

  @Test
  void validate_LastAccessManagerRemoved_ValidationFails() {
    var form = new TeamMemberRolesForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");

    var team = TeamTestUtil.Builder().build();
    var teamMember = TeamMemberTestUtil.Builder().build();
    var dto = new IndustryTeamMemberEditRolesValidatorDto(team, teamMember);

    form.setRoles(Set.of(IndustryTeamRole.SCAP_SUBMITTER.name(),
        IndustryTeamRole.SCAP_VIEWER.name(),
        "THIS IS NOT A VALID INDUSTRY ROLE"));

    industryTeamMemberEditRolesValidator.validate(form, bindingResult, dto);
    assertThat(bindingResult.getAllErrors()).hasSize(2);
    var codes = ValidatorTestingUtil.extractErrors(bindingResult);
    assertTrue(codes.get("roles").contains("roles.accessManagerRequired"));
  }

  @Test
  void validate_formEmpty_ValidationFails() {
    var form = new TeamMemberRolesForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");

    var team = TeamTestUtil.Builder().build();
    var teamMember = TeamMemberTestUtil.Builder().build();
    var dto = new IndustryTeamMemberEditRolesValidatorDto(team, teamMember);

    industryTeamMemberEditRolesValidator.validate(form, bindingResult, dto);
    assertThat(bindingResult.getAllErrors()).hasSize(1);
  }

}
