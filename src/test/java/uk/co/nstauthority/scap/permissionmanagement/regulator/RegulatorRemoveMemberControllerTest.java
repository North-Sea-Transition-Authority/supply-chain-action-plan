package uk.co.nstauthority.scap.permissionmanagement.regulator;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.csrf;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.model;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.view;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;
import static uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberRemovalService.LAST_ACCESS_MANAGER_ERROR_MESSAGE;
import static uk.co.nstauthority.scap.utils.ControllerTestingUtil.redirectUrl;

import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.web.servlet.mvc.support.RedirectAttributesModelMap;
import uk.co.fivium.digital.energyportalteamaccesslibrary.team.EnergyPortalAccessService;
import uk.co.fivium.digital.energyportalteamaccesslibrary.team.InstigatingWebUserAccountId;
import uk.co.fivium.digital.energyportalteamaccesslibrary.team.ResourceType;
import uk.co.fivium.digital.energyportalteamaccesslibrary.team.TargetWebUserAccountId;
import uk.co.nstauthority.scap.energyportal.WebUserAccountId;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.permissionmanagement.TeamId;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberRemovalService;

@WithMockUser
@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes = RegulatorRemoveMemberController.class)
class RegulatorRemoveMemberControllerTest extends AbstractRegulatorTeamControllerTest{

  @MockBean
  private TeamMemberRemovalService teamMemberRemovalService;

  @MockBean
  private EnergyPortalAccessService energyPortalAccessService;

  private static final WebUserAccountId wuaId = new WebUserAccountId(1000L);

  @Test
  void renderRemoveMember_noTeamFound_RedirectsToMemberList() throws Exception {
    canRemoveTeamMember();
    when(teamMemberService.findTeamMember(team, wuaId)).thenReturn(Optional.empty());

    mockMvc.perform(get(ReverseRouter.route(on(RegulatorRemoveMemberController.class)
            .renderRemoveMember(teamId, wuaId))))
        .andExpect(status().is3xxRedirection())
        .andExpect(redirectUrl("/permission-management/regulator/%s".formatted(teamId.uuid().toString())));
  }

  @Test
  void renderRemoveMember_teamFound_RenderRemoveForm() throws Exception {
    mockMvc.perform(get(ReverseRouter.route(on(RegulatorRemoveMemberController.class)
            .renderRemoveMember(teamId, webUserAccountId))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/permissionmanagement/removeTeamMember"));
  }

  @Test
  void removeMember_cannotRemoveTeamMember_AddsErrorMessageToRenderRemoveMember() throws Exception {
    var teamId = new TeamId(UUID.randomUUID());

    mockMvc.perform(post(ReverseRouter.route(on(RegulatorRemoveMemberController.class)
            .removeMember(teamId,
                webUserAccountId,
                new RedirectAttributesModelMap())))
            .with(csrf()))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/permissionmanagement/removeTeamMember"))
        .andExpect(model().attribute("singleErrorMessage", LAST_ACCESS_MANAGER_ERROR_MESSAGE));
  }

  @Test
  void removeMember_canRemoveTeamMember_RemoveMember() throws Exception {
    canRemoveTeamMember();
    mockMvc.perform(post(ReverseRouter.route(on(RegulatorRemoveMemberController.class)
            .removeMember(teamId,
                webUserAccountId,
                new RedirectAttributesModelMap())))
            .with(csrf()))
        .andExpect(status().is3xxRedirection())
        .andExpect(redirectUrl("/permission-management/regulator/%s".formatted(teamId.uuid().toString())));
  }

  @Test
  void removeMember_LastTeam_RemovesEnergyPortalAccess() throws Exception {
    canRemoveTeamMember();
    when(teamMemberService.getAllPermissionsForUser(testUser.wuaId())).thenReturn(Collections.emptyList());

    mockMvc.perform(post(ReverseRouter.route(on(RegulatorRemoveMemberController.class)
            .removeMember(teamId,
                webUserAccountId,
                new RedirectAttributesModelMap())))
            .with(csrf()))
        .andExpect(status().is3xxRedirection())
        .andExpect(redirectUrl("/permission-management/regulator/%s".formatted(teamId.uuid().toString())));

    verify(energyPortalAccessService).removeUserFromAccessTeam(any(ResourceType.class),
        any(TargetWebUserAccountId.class),
        any(InstigatingWebUserAccountId.class));
  }

  @Test
  void removeMember_StillInTeam_NoCallToEPAccess() throws Exception {
    canRemoveTeamMember();
    when(teamMemberService.getAllPermissionsForUser(anyLong())).thenReturn(List.of(RolePermission.SUBMIT_SCAP));

    mockMvc.perform(post(ReverseRouter.route(on(RegulatorRemoveMemberController.class)
            .removeMember(teamId,
                webUserAccountId,
                new RedirectAttributesModelMap())))
            .with(csrf()))
        .andExpect(status().is3xxRedirection())
        .andExpect(redirectUrl("/permission-management/regulator/%s".formatted(teamId.uuid().toString())));

    verify(energyPortalAccessService, never()).removeUserFromAccessTeam(any(ResourceType.class),
        any(TargetWebUserAccountId.class),
        any(InstigatingWebUserAccountId.class));
  }

  @Test
  void removeMember_noTeam_redirectsToMemberList() throws Exception {
    //noinspection unchecked
    when(teamMemberService.findTeamMember(any(), any())).thenReturn(Optional.of(teamMember),Optional.empty());
    mockMvc.perform(post(ReverseRouter.route(on(RegulatorRemoveMemberController.class)
            .removeMember(teamId,
                webUserAccountId,
                new RedirectAttributesModelMap())))
            .with(csrf()))
        .andExpect(status().is3xxRedirection())
        .andExpect(redirectUrl("/permission-management/regulator/%s".formatted(teamId.uuid().toString())));
  }

  private void canRemoveTeamMember() {
    when(teamMemberService.findTeamMember(team, wuaId)).thenReturn(Optional.of(teamMember));
    when(teamMemberRemovalService.canRemoveTeamMember(any(), any())).thenReturn(true);
  }
}
