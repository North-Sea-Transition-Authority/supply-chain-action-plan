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
import static uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberRemovalService.LAST_ACCESS_MANAGER_ERROR_MESSAGE;

import java.util.Optional;
import java.util.UUID;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.web.servlet.mvc.support.RedirectAttributesModelMap;
import uk.co.nstauthority.scap.energyportal.WebUserAccountId;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.TeamId;
import uk.co.nstauthority.scap.permissionmanagement.TeamTestUtil;
import uk.co.nstauthority.scap.permissionmanagement.TeamType;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberRemovalService;

@WithMockUser
@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes = IndustryRemoveMemberController.class)
class IndustryRemoveMemberControllerTest extends AbstractIndustryTeamControllerTest{

  @MockBean
  private TeamMemberRemovalService teamMemberRemovalService;

  @Test
  void renderRemoveMember_noTeamFound_RedirectsToMemberList() throws Exception {
    var wuaId = new WebUserAccountId(1000L);
    when(teamMemberService.findTeamMember(team, wuaId)).thenReturn(Optional.empty());

    mockMvc.perform(get(
            ReverseRouter.route(on(IndustryRemoveMemberController.class)
                .renderRemoveMember(teamId, wuaId))))
        .andExpect(status().is3xxRedirection())
        .andExpect(view().name("redirect:/permission-management/industry/%s".formatted(teamId.uuid().toString())));
  }

  @Test
  void renderRemoveMember_teamFound_RenderRemoveForm() throws Exception {
    var wuaId = new WebUserAccountId(1000L);
    when(teamMemberService.findTeamMember(team, wuaId)).thenReturn(Optional.of(teamMember));

    mockMvc.perform(get(
        ReverseRouter.route(on(IndustryRemoveMemberController.class)
            .renderRemoveMember(teamId, wuaId))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/permissionmanagement/removeTeamMember"));
  }

  @Test
  void removeMember_cannotRemoveTeamMember_AddsErrorMessageToRenderRemoveMember() throws Exception {
    var wuaId = new WebUserAccountId(1000L);
    when(teamMemberService.findTeamMember(team, wuaId)).thenReturn(Optional.of(teamMember));

    mockMvc.perform(post(
            ReverseRouter.route(on(IndustryRemoveMemberController.class)
                .removeMember(teamId,
                    wuaId,
                    new RedirectAttributesModelMap())))
            .with(csrf()))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/permissionmanagement/removeTeamMember"))
        .andExpect(model().attribute("singleErrorMessage", LAST_ACCESS_MANAGER_ERROR_MESSAGE));
  }

  @Test
  void removeMember_canRemoveTeamMember_RemovesMember() throws Exception {
    when(teamMemberRemovalService.canRemoveTeamMember(any(), any())).thenReturn(true);
    mockMvc.perform(post(
            ReverseRouter.route(on(IndustryRemoveMemberController.class)
                .removeMember(teamId,
                    webUserAccountId,
                    new RedirectAttributesModelMap())))
            .with(csrf()))
        .andExpect(status().is3xxRedirection())
        .andExpect(view().name("redirect:/permission-management/industry/%s".formatted(teamId.uuid().toString())));
  }

  @Test
  void removeMember_noTeam_redirectsToMemberList() throws Exception {
    var teamId = new TeamId(UUID.randomUUID());
    var wuaId = new WebUserAccountId(1000L);
    var team = TeamTestUtil.Builder()
        .withTeamType(TeamType.INDUSTRY)
        .build();

    when(teamService.getTeam(teamId)).thenReturn(team);

    mockMvc.perform(post(
            ReverseRouter.route(on(IndustryRemoveMemberController.class)
                .removeMember(teamId,
                    wuaId,
                    new RedirectAttributesModelMap())))
            .with(csrf()))
        .andExpect(status().is3xxRedirection())
        .andExpect(view().name("redirect:/permission-management/industry/%s".formatted(teamId.uuid().toString())));
  }
}
