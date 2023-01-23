package uk.co.nstauthority.scap.permissionmanagement.regulator;

import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.model;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.view;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;
import static uk.co.nstauthority.scap.authentication.TestUserProvider.user;

import org.junit.jupiter.api.Test;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.ContextConfiguration;
import uk.co.nstauthority.scap.controllerhelper.ControllerHelperService;
import uk.co.nstauthority.scap.energyportal.EnergyPortalUserService;
import uk.co.nstauthority.scap.enumutil.DisplayableEnumOptionUtil;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.TeamId;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberTestUtil;
import uk.co.nstauthority.scap.permissionmanagement.TeamTestUtil;
import uk.co.nstauthority.scap.utils.EnergyPortalUserDtoTestUtil;


@ContextConfiguration(classes = RegulatorAddRolesController.class)
class RegulatorAddRolesControllerTest extends AbstractRegulatorTeamControllerTest {

  @MockBean
  ControllerHelperService controllerHelperService;

  @MockBean
  EnergyPortalUserService energyPortalUserService;

  @MockBean
  RegulatorTeamMemberRolesValidator industryTeamMemberRolesValidator;

  @Test
  void industryAddController_renderAddRoles_noAuthorisation() throws Exception {
    var team = TeamTestUtil.Builder().build();

    var user = TeamMemberTestUtil.Builder().build();

    mockMvc.perform(
        get(ReverseRouter.route(on(RegulatorAddRolesController.class).renderAddTeamMemberRoles(
            new TeamId(team.getUuid()),
            user.wuaId()))))
        .andExpect(status().isUnauthorized());
  }

  @Test
  void industryAddController_renderAddRoles_Authorisation() throws Exception {
    var energyPortalUserDto = EnergyPortalUserDtoTestUtil.Builder().build();
    when(energyPortalUserService.getEnergyPortalUser(webUserAccountId)).thenReturn(energyPortalUserDto);

    mockMvc.perform(
            get(ReverseRouter.route(on(RegulatorAddRolesController.class).renderAddTeamMemberRoles(
                teamId,
                webUserAccountId)))
                .with(user(user)))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/permissionmanagement/teamMemberRoles"))
        .andExpect(model().attribute("roles", DisplayableEnumOptionUtil.getDisplayableOptionsWithDescription(RegulatorTeamRole.class)));
  }
}
