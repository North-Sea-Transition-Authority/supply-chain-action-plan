package uk.co.nstauthority.scap.permissionmanagement.industry;

import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.csrf;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.view;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import org.junit.jupiter.api.Test;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.validation.BeanPropertyBindingResult;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberRolesForm;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberRoleService;

@ContextConfiguration(classes = IndustryEditMemberController.class)
class IndustryEditMemberControllerTest extends AbstractIndustryTeamControllerTest {

  @MockBean
  TeamMemberRoleService teamMemberRoleService;

  @MockBean
  IndustryTeamMemberEditRolesValidator industryTeamMemberEditRolesValidator;

  @Test
  void renderEditMember() throws Exception {
    mockMvc.perform(get(ReverseRouter.route(on(IndustryEditMemberController.class)
        .renderEditMember(teamId, webUserAccountId)))
        .with(authenticatedScapUser()))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/permissionmanagement/AddTeamMemberRoles"));
  }

  @Test
  void editMemberSucceeds_routesToMemberList() throws Exception {

    var form = new TeamMemberRolesForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");

    mockMvc.perform(post(ReverseRouter.route(on(IndustryEditMemberController.class)
        .editMember(teamId, webUserAccountId, form, bindingResult)))
        .with(authenticatedScapUser())
        .with(csrf()))
        .andExpect(status().is3xxRedirection())
        .andExpect(view().name("redirect:/permission-management/industry/%s"
            .formatted(teamId.uuid())));
  }
}
