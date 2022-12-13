package uk.co.nstauthority.scap.permissionmanagement;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.csrf;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.model;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.view;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;
import static uk.co.nstauthority.scap.authentication.TestUserProvider.user;
import static uk.co.nstauthority.scap.mvc.ReverseRouter.emptyBindingResult;

import java.util.List;
import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.validation.BeanPropertyBindingResult;
import uk.co.fivium.energyportalapi.generated.types.OrganisationGroup;
import uk.co.nstauthority.scap.AbstractControllerTest;
import uk.co.nstauthority.scap.authentication.ServiceUserDetailTestUtil;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.teams.NewTeamForm;
import uk.co.nstauthority.scap.permissionmanagement.teams.NewTeamFormvalidator;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamManagementController;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamView;
import uk.co.nstauthority.scap.scap.organisationgroup.OrganisationGroupService;

@ContextConfiguration(classes = TeamManagementController.class)
class TeamManagementControllerTest extends AbstractControllerTest {

  @MockBean
  private TeamService teamService;

  @MockBean
  private OrganisationGroupService organisationGroupService;

  @MockBean
  private NewTeamFormvalidator newTeamFormvalidator;

  @Test
  void renderTeamList_notAuthorised_thenUnAuthorised() throws Exception {
    mockMvc.perform(
        get(ReverseRouter.route(on(TeamManagementController.class).renderTeamList())))
        .andExpect(status().isUnauthorized());
  }

  @Test
  void renderTeamList_authorised_thenOk() throws Exception {
    var user = ServiceUserDetailTestUtil.Builder().build();
    mockMvc.perform(
            get(ReverseRouter.route(on(TeamManagementController.class).renderTeamList()))
              .with(user(user)))
        .andExpect(status().isOk());
  }

  @Test
  void renderTeamList_userInTeam_thenTeamPresent() throws Exception {
    var user = ServiceUserDetailTestUtil.Builder().build();
    var team = TeamTestUtil
        .Builder()
        .withTeamType(TeamType.INDUSTRY)
        .build();

    when(userDetailService.getUserDetail()).thenReturn(user);
    when(teamService.getTeamsThatUserBelongsTo(user)).thenReturn(List.of(team));

    mockMvc.perform(
        get(ReverseRouter.route(on(TeamManagementController.class).renderTeamList()))
            .with(user(user)))
        .andExpect(status().isOk())
        .andExpect(model().attribute("allTeams", List.of(TeamView.fromTeam(team))));
  }

  @Test
  void renderNewTeamForm_authorisedUser_isOk() throws Exception {
    var user = ServiceUserDetailTestUtil.Builder().build();

    mockMvc.perform(
        get(ReverseRouter.route(on(TeamManagementController.class).renderNewIndustryTeamForm(new NewTeamForm())))
            .with(user(user)))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/permissionmanagement/addTeam"))
    ;
  }

  @Test
  void addNewIndustryTeam_FormValidates_ReturnToTeamList() throws Exception {
    var user = ServiceUserDetailTestUtil.Builder().withWuaId(100L).build();

    var orgGroup = new OrganisationGroup();
    orgGroup.setName("Royal Dutch Shell");
    orgGroup.setOrganisationGroupId(10000);

    var form = new NewTeamForm();
    form.setOrganisationGroupId("10000");

    when(teamService.validate(any(), any())).thenReturn(emptyBindingResult());
    when(organisationGroupService.getOrganisationGroupById(any(), any())).thenReturn(Optional.of(orgGroup));when(userDetailService.getUserDetail()).thenReturn(user);
    when(userDetailService.getUserDetail()).thenReturn(user);

    mockMvc.perform(
            post(ReverseRouter.route(on(TeamManagementController.class).addNewIndustryTeam(form, emptyBindingResult())))
                .flashAttr("form", form)
                .with(csrf())
                .with(user(user)))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/permissionmanagement/teamList"));
  }

  @Test
  void addNewIndustryTeam_FormValidatesGroupDoesntExist_ReturnToNewTeam() throws Exception {
    var user = ServiceUserDetailTestUtil.Builder().withWuaId(100L).build();

    var form = new NewTeamForm();
    form.setOrganisationGroupId("10000");

    when(teamService.validate(any(), any())).thenReturn(emptyBindingResult());
    when(userDetailService.getUserDetail()).thenReturn(user);

    mockMvc.perform(
            post(ReverseRouter.route(on(TeamManagementController.class).addNewIndustryTeam(form, emptyBindingResult())))
                .flashAttr("form", form)
                .with(csrf())
                .with(user(user)))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/permissionmanagement/addTeam"));
  }
}
