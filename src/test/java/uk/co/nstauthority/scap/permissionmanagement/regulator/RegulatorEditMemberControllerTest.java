package uk.co.nstauthority.scap.permissionmanagement.regulator;

import static org.mockito.Mockito.when;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.csrf;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.view;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;
import static uk.co.nstauthority.scap.authentication.TestUserProvider.user;

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
import uk.co.nstauthority.scap.energyportal.WebUserAccountId;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.TeamId;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberRolesForm;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberTestUtil;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberViewService;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberViewTestUtil;
import uk.co.nstauthority.scap.permissionmanagement.TeamTestUtil;
import uk.co.nstauthority.scap.permissionmanagement.industry.IndustryTeamRole;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberRoleService;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberService;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;

@ContextConfiguration(classes = RegulatorEditMemberController.class)
class RegulatorEditMemberControllerTest extends AbstractRegulatorTeamControllerTest {

  @MockBean
  TeamMemberRoleService teamMemberRoleService;

  @MockBean
  RegulatorTeamMemberEditRolesValidator regulatorTeamMemberEditRolesValidator;

  @MockBean
  RegulatorTeamService regulatorTeamService;

  private TeamId teamId = new TeamId(UUID.randomUUID());

  private ServiceUserDetail user;

  @BeforeEach
  void setup() {
    var teamMember = TeamMemberTestUtil
        .Builder()
        .withRole(IndustryTeamRole.SCAP_SUBMITTER)
        .build();
    var teamMemberView = TeamMemberViewTestUtil.Builder().build();
    user = ServiceUserDetailTestUtil.Builder().build();

    when(regulatorTeamService.getTeam(user)).thenReturn(team);
    when(teamMemberViewService.getTeamMemberViewOrThrow(teamMember)).thenReturn(teamMemberView);
  }

  @Test
  void renderEditMember() throws Exception {
    mockMvc.perform(get(ReverseRouter.route(on(RegulatorEditMemberController.class)
        .renderEditMember(teamId, webUserAccountId)))
        .with(user(user)))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/permissionmanagement/AddTeamMemberRoles"));
  }

  @Test
  void editMemberSucceeds_routesToMemberList() throws Exception {

    var form = new TeamMemberRolesForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");

    mockMvc.perform(post(ReverseRouter.route(on(RegulatorEditMemberController.class)
        .editMember(teamId, webUserAccountId, form, bindingResult)))
        .with(user(user))
        .with(csrf()))
        .andExpect(status().is3xxRedirection())
        .andExpect(view().name("redirect:/permission-management/regulator/%s"
            .formatted(teamId.uuid())));
  }
}
