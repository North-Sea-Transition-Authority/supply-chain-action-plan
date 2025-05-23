package uk.co.nstauthority.scap.permissionmanagement.industry;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.csrf;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.view;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;
import static uk.co.nstauthority.scap.utils.ControllerTestingUtil.redirectUrl;

import java.util.Collections;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.validation.BeanPropertyBindingResult;
import uk.co.fivium.digital.energyportalteamaccesslibrary.team.EnergyPortalAccessService;
import uk.co.fivium.digital.energyportalteamaccesslibrary.team.InstigatingWebUserAccountId;
import uk.co.fivium.digital.energyportalteamaccesslibrary.team.ResourceType;
import uk.co.fivium.digital.energyportalteamaccesslibrary.team.TargetWebUserAccountId;
import uk.co.nstauthority.scap.energyportal.EnergyPortalUserDto;
import uk.co.nstauthority.scap.energyportal.EnergyPortalUserService;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.AddTeamMemberForm;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.permissionmanagement.teams.AddTeamMemberValidator;
import uk.co.nstauthority.scap.utils.EnergyPortalUserDtoTestUtil;

@ContextConfiguration(classes = IndustryAddMemberController.class)
class IndustryAddMemberControllerTest extends AbstractIndustryTeamControllerTest {

  @MockBean
  AddTeamMemberValidator addTeamMemberValidator;

  @MockBean
  EnergyPortalUserService energyPortalUserService;

  @MockBean
  EnergyPortalAccessService energyPortalAccessService;

  private static final EnergyPortalUserDto energyPortalDto = EnergyPortalUserDtoTestUtil.Builder().build();

  private AddTeamMemberForm form;

  private BeanPropertyBindingResult bindingResult;

  private void setupMocks() {
    form = new AddTeamMemberForm();
    bindingResult = new BeanPropertyBindingResult(form, "form");
    when(energyPortalUserService.findUsersByUsername(any())).thenReturn(List.of(energyPortalDto));
  }

  @Test
  void renderAddMember() throws Exception {
    setupMocks();
    mockMvc.perform(get(ReverseRouter.route(on(IndustryAddMemberController.class)
        .renderAddTeamMember(teamId)))
        .with(authenticatedScapUser()))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/permissionmanagement/AddTeamMember"));
  }

  @Test
  void addMemberToTeam_ValidationSucceeds_rendersAddRoles() throws Exception {
    setupMocks();
    mockMvc.perform(post(ReverseRouter.route(on(IndustryAddMemberController.class)
            .addMemberToTeamSubmission(teamId, form, bindingResult)))
            .with(authenticatedScapUser())
            .with(csrf()))
        .andExpect(status().is3xxRedirection())
        .andExpect(redirectUrl("/permission-management/industry/%s/add-member/%s/roles"
            .formatted(teamId.uuid().toString(), energyPortalDto.webUserAccountId())));
  }

  @Test
  void addMemberToTeam_FirstTeam_GetEnergyPortalAccess()  throws Exception {
    setupMocks();
    when(teamMemberService.getAllPermissionsForUser(testUser.wuaId())).thenReturn(Collections.emptyList());

    mockMvc.perform(post(ReverseRouter.route(on(IndustryAddMemberController.class)
            .addMemberToTeamSubmission(teamId, form, bindingResult)))
            .with(authenticatedScapUser())
            .with(csrf()))
        .andExpect(status().is3xxRedirection())
        .andExpect(redirectUrl("/permission-management/industry/%s/add-member/%s/roles"
            .formatted(teamId.uuid().toString(), energyPortalDto.webUserAccountId())));

    verify(energyPortalAccessService).addUserToAccessTeam(any(ResourceType.class),
        any(TargetWebUserAccountId.class),
        any(InstigatingWebUserAccountId.class));
  }

  @Test
  void addMemberToTeam_AlreadyHasAccess_NoCallToEPAccess()  throws Exception {
    setupMocks();
    when(teamMemberService.getAllPermissionsForUser(anyLong())).thenReturn(List.of(RolePermission.SUBMIT_SCAP));

    mockMvc.perform(post(ReverseRouter.route(on(IndustryAddMemberController.class)
            .addMemberToTeamSubmission(teamId, form, bindingResult)))
            .with(authenticatedScapUser())
            .with(csrf()))
        .andExpect(status().is3xxRedirection())
        .andExpect(redirectUrl("/permission-management/industry/%s/add-member/%s/roles"
            .formatted(teamId.uuid().toString(), energyPortalDto.webUserAccountId())));

    verify(energyPortalAccessService, never()).addUserToAccessTeam(any(ResourceType.class),
        any(TargetWebUserAccountId.class),
        any(InstigatingWebUserAccountId.class));
  }
}
