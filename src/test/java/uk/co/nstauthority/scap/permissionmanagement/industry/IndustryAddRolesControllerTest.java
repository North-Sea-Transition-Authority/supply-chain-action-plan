package uk.co.nstauthority.scap.permissionmanagement.industry;

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
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.validation.BeanPropertyBindingResult;
import uk.co.nstauthority.scap.AbstractControllerTest;
import uk.co.nstauthority.scap.authentication.ServiceUserDetail;
import uk.co.nstauthority.scap.authentication.ServiceUserDetailTestUtil;
import uk.co.nstauthority.scap.controllerhelper.ControllerHelperService;
import uk.co.nstauthority.scap.energyportal.EnergyPortalUserDto;
import uk.co.nstauthority.scap.energyportal.EnergyPortalUserService;
import uk.co.nstauthority.scap.enumutil.DisplayableEnumOptionUtil;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.Team;
import uk.co.nstauthority.scap.permissionmanagement.TeamId;
import uk.co.nstauthority.scap.permissionmanagement.TeamMember;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberRolesForm;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberTestUtil;
import uk.co.nstauthority.scap.permissionmanagement.TeamTestUtil;

import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;
import uk.co.nstauthority.scap.utils.EnergyPortalUserDtoTestUtil;

@ContextConfiguration(classes = IndustryAddRolesController.class)
class IndustryAddRolesControllerTest extends AbstractControllerTest {

  @MockBean
  TeamService teamService;

  @MockBean
  ControllerHelperService controllerHelperService;

  @MockBean
  EnergyPortalUserService energyPortalUserService;

  @MockBean
  IndustryTeamMemberRolesValidator industryTeamMemberRolesValidator;

  private Team team;
  private ServiceUserDetail serviceDetailUser;
  private TeamMember user;

  @BeforeEach
  void setup() {
    team = TeamTestUtil.Builder().build();
    serviceDetailUser = ServiceUserDetailTestUtil.Builder().build();
    user = TeamMemberTestUtil.Builder().build();
  }

  @Test
  void industryAddController_renderAddRoles_noAuthorisation() throws Exception {
    var team = TeamTestUtil.Builder().build();

    var user = TeamMemberTestUtil.Builder().build();

    mockMvc.perform(
        get(ReverseRouter.route(on(IndustryAddRolesController.class).renderAddTeamMemberRoles(
            new TeamId(team.getUuid()),
            user.wuaId()))))
        .andExpect(status().isUnauthorized());
  }

  @Test
  void industryAddController_renderAddRoles_Authorisation() throws Exception {
    var energyPortalUserDto = EnergyPortalUserDtoTestUtil.Builder().build();
    when(energyPortalUserService.getEnergyPortalUser(user.wuaId())).thenReturn(energyPortalUserDto);

    mockMvc.perform(
        get(ReverseRouter.route(on(IndustryAddRolesController.class).renderAddTeamMemberRoles(
            new TeamId(team.getUuid()),
            user.wuaId())))
            .with(user(serviceDetailUser)))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/permissionmanagement/teamMemberRoles"))
        .andExpect(model().attribute("roles", DisplayableEnumOptionUtil.getDisplayableOptionsWithDescription(IndustryTeamRole.class)));
  }
}
