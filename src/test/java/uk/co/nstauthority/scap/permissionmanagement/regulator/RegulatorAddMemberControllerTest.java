package uk.co.nstauthority.scap.permissionmanagement.regulator;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.csrf;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.view;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;
import static uk.co.nstauthority.scap.authentication.TestUserProvider.user;

import java.util.List;
import java.util.Optional;
import java.util.UUID;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.validation.BeanPropertyBindingResult;
import uk.co.nstauthority.scap.AbstractControllerTest;
import uk.co.nstauthority.scap.authentication.ServiceUserDetail;
import uk.co.nstauthority.scap.authentication.ServiceUserDetailTestUtil;
import uk.co.nstauthority.scap.configuration.SamlProperties;
import uk.co.nstauthority.scap.energyportal.EnergyPortalUserService;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.AddTeamMemberForm;
import uk.co.nstauthority.scap.permissionmanagement.TeamId;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberTestUtil;
import uk.co.nstauthority.scap.permissionmanagement.industry.IndustryTeamRole;
import uk.co.nstauthority.scap.permissionmanagement.teams.AddTeamMemberValidator;
import uk.co.nstauthority.scap.utils.EnergyPortalUserDtoTestUtil;

@ContextConfiguration(classes = RegulatorAddMemberController.class)
class RegulatorAddMemberControllerTest extends AbstractRegulatorTeamControllerTest {

  @MockBean
  AddTeamMemberValidator addTeamMemberValidator;

  @MockBean
  EnergyPortalUserService energyPortalUserService;

  @MockBean
  SamlProperties samlProperties;


  @Test
  void renderAddMember() throws Exception {
    when(samlProperties.getRegistrationUrl()).thenReturn("test-test.com");

    mockMvc.perform(get(ReverseRouter.route(on(RegulatorAddMemberController.class)
        .renderAddTeamMember(teamId)))
        .with(user(user))
        .with(csrf()))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/permissionmanagement/AddTeamMember"));
  }

  @Test
  void addMemberToTeam_ValidationSucceeds_rendersAddRoles() throws Exception {
    var energyPortalDto = EnergyPortalUserDtoTestUtil.Builder().build();
    var form = new AddTeamMemberForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");

    when(samlProperties.getRegistrationUrl()).thenReturn("test-test.com");
    when(energyPortalUserService.findUsersByUsername(any())).thenReturn(List.of(energyPortalDto));

    mockMvc.perform(post(ReverseRouter.route(on(RegulatorAddMemberController.class)
            .addMemberToTeamSubmission(teamId, form, bindingResult)))
            .with(user(user))
            .with(csrf()))
        .andExpect(status().is3xxRedirection())
        .andExpect(view().name(
            "redirect:/permission-management/regulator/%s/add-member/%s/roles"
                .formatted(teamId.uuid().toString(), energyPortalDto.webUserAccountId())));


  }

  private ServiceUserDetail getUser() {
    return ServiceUserDetailTestUtil.Builder()
        .withWuaId(100L)
        .build();
  }
}
