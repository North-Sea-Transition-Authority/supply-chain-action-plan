package uk.co.nstauthority.scap.permissionmanagement.regulator;

import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.model;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.view;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;
import static uk.co.nstauthority.scap.authentication.TestUserProvider.user;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.bean.override.mockito.MockitoBean;
import uk.co.nstauthority.scap.controllerhelper.ControllerHelperService;
import uk.co.nstauthority.scap.energyportal.EnergyPortalUserService;
import uk.co.nstauthority.scap.energyportal.user.AllowedDomainService;
import uk.co.nstauthority.scap.enumutil.DisplayableEnumOptionUtil;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.TeamId;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberTestUtil;
import uk.co.nstauthority.scap.permissionmanagement.TeamTestUtil;
import uk.co.nstauthority.scap.utils.EnergyPortalUserDtoTestUtil;

@ContextConfiguration(classes = RegulatorAddRolesController.class)
class RegulatorAddRolesControllerTest extends AbstractRegulatorTeamControllerTest {

  @MockitoBean
  ControllerHelperService controllerHelperService;

  @MockitoBean
  EnergyPortalUserService energyPortalUserService;

  @MockitoBean
  RegulatorTeamMemberRolesValidator industryTeamMemberRolesValidator;

  @MockitoBean
  AllowedDomainService allowedDomainService;

  @Test
  void industryAddController_renderAddRoles_noAuthorisation() throws Exception {
    var team = TeamTestUtil.Builder().build();

    var user = TeamMemberTestUtil.Builder().build();

    mockMvc.perform(
        get(ReverseRouter.route(on(RegulatorAddRolesController.class).renderAddTeamMemberRoles(
            new TeamId(team.getUuid()),
            user.wuaId()))))
        .andExpect(status().is3xxRedirection());
  }

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void industryAddController_renderAddRoles_Authorisation(boolean isAllowed) throws Exception {
    var energyPortalUserDto = EnergyPortalUserDtoTestUtil.Builder().build();
    when(energyPortalUserService.getEnergyPortalUser(webUserAccountId)).thenReturn(energyPortalUserDto);
    when(allowedDomainService.isAllowedDomain(energyPortalUserDto.emailAddress(), team)).thenReturn(
        isAllowed
    );

    mockMvc.perform(
            get(ReverseRouter.route(on(RegulatorAddRolesController.class).renderAddTeamMemberRoles(
                teamId,
                webUserAccountId)))
                .with(user(user)))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/permissionmanagement/teamMemberRoles"))
        .andExpect(model().attribute("userHasAllowedEmail", isAllowed))
        .andExpect(model().attribute("roles", DisplayableEnumOptionUtil.getDisplayableOptionsWithDescription(RegulatorTeamRole.class)));
  }
}
