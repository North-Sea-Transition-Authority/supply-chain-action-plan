package uk.co.nstauthority.scap.permissionmanagement.teams;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Optional;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.validation.BeanPropertyBindingResult;
import org.springframework.validation.BindingResult;
import uk.co.fivium.energyportalapi.generated.types.OrganisationGroup;
import uk.co.nstauthority.scap.permissionmanagement.TeamTestUtil;
import uk.co.nstauthority.scap.scap.organisationgroup.OrganisationGroupService;

@ExtendWith(MockitoExtension.class)
class NewTeamFormValidatorTest {

  @Mock
  OrganisationGroupService organisationGroupService;

  @Mock
  TeamService teamService;

  @InjectMocks
  NewTeamFormValidator validator;

  private NewTeamForm form;

  private BindingResult bindingResult;

  @BeforeEach
  void setup() {
    form = new NewTeamForm();
    bindingResult = new BeanPropertyBindingResult(form, "form");
  }

  @Test
  void validator_NotValidId_ReturnError() {
    form.setOrganisationGroupId("THIS IS NOT A VALID ID");

    validator.validate(form, bindingResult);
    assertThat(bindingResult.getFieldError("organisationGroupId.inputValue")).isNotNull();
    assertThat(bindingResult.getFieldError("organisationGroupId.inputValue").getCodes()).contains("organisationGroupId.invalid");

    verify(teamService, times(0)).findByEnergyPortalOrgGroupId(anyInt());
    verify(organisationGroupService, times(0)).getOrganisationGroupById(any(), any());
  }

  @Test
  void validator_AlreadyExists_ReturnError() {
    form.setOrganisationGroupId("1000");

    var team = TeamTestUtil.Builder().build();
    when(teamService.findByEnergyPortalOrgGroupId(1000)).thenReturn(Optional.of(team));

    validator.validate(form, bindingResult);
    assertThat(bindingResult.getFieldError("organisationGroupId.inputValue")).isNotNull();
    assertThat(bindingResult.getFieldError("organisationGroupId.inputValue").getCodes()).contains("organisationGroupId.alreadyExists");

    verify(teamService).findByEnergyPortalOrgGroupId(1000);
    verify(organisationGroupService, times(0)).getOrganisationGroupById(any(), any());
  }

  @Test
  void validator_NoEnergyPortalOrganisation_ReturnError() {
    form.setOrganisationGroupId("1000");

    when(teamService.findByEnergyPortalOrgGroupId(1000)).thenReturn(Optional.empty());
    when(organisationGroupService.getOrganisationGroupById(eq(1000), any())).thenReturn(Optional.empty());

    validator.validate(form, bindingResult);
    assertThat(bindingResult.getFieldError("organisationGroupId.inputValue")).isNotNull();
    assertThat(bindingResult.getFieldError("organisationGroupId.inputValue").getCodes()).contains("organisationGroupId.doesNotExist");

    verify(teamService).findByEnergyPortalOrgGroupId(1000);
    verify(organisationGroupService).getOrganisationGroupById(any(), any());
  }



  @Test
  void validator_AllValid_NoErrors() {
    form.setOrganisationGroupId("1000");

    var orgGroup = new OrganisationGroup();
    when(teamService.findByEnergyPortalOrgGroupId(1000)).thenReturn(Optional.empty());
    when(organisationGroupService.getOrganisationGroupById(eq(1000), any())).thenReturn(Optional.of(orgGroup));

    validator.validate(form, bindingResult);
    assertThat(bindingResult.getFieldError("organisationGroupId.inputValue")).isNull();

    verify(teamService).findByEnergyPortalOrgGroupId(1000);
    verify(organisationGroupService).getOrganisationGroupById(any(), any());
  }
}
