package uk.co.nstauthority.scap.permissionmanagement.industry;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
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

import uk.co.nstauthority.scap.utils.EnergyPortalUserDtoTestUtil;

@ContextConfiguration(classes = IndustryAddRolesController.class)
class IndustryAddRolesControllerTest extends AbstractIndustryTeamControllerTest {

  @MockBean
  ControllerHelperService controllerHelperService;

  @MockBean
  EnergyPortalUserService energyPortalUserService;

  @MockBean
  IndustryTeamMemberRolesValidator industryTeamMemberRolesValidator;

  @Test
  void industryAddController_renderAddRoles_noAuthorisation() throws Exception {
    mockMvc.perform(
        get(ReverseRouter.route(on(IndustryAddRolesController.class).renderAddTeamMemberRoles(
            teamId,
            webUserAccountId))))
        .andExpect(status().isUnauthorized());
  }

  @Test
  void industryAddController_renderAddRoles_Authorisation() throws Exception {
    var energyPortalUserDto = EnergyPortalUserDtoTestUtil.Builder().build();
    when(energyPortalUserService.getEnergyPortalUser(webUserAccountId)).thenReturn(energyPortalUserDto);

    mockMvc.perform(
        get(ReverseRouter.route(on(IndustryAddRolesController.class).renderAddTeamMemberRoles(
            teamId,
            webUserAccountId)))
            .with(user(user)))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/permissionmanagement/teamMemberRoles"))
        .andExpect(model().attribute("roles", DisplayableEnumOptionUtil.getDisplayableOptionsWithDescription(IndustryTeamRole.class)));
  }
}
