package uk.co.nstauthority.scap.workarea;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.model;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.view;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ContextConfiguration;
import uk.co.nstauthority.scap.AbstractControllerTest;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberRoleTestUtil;
import uk.co.nstauthority.scap.scap.start.ScapStartController;

@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes = WorkAreaController.class)
@WithMockUser
class WorkAreaControllerTest extends AbstractControllerTest {

  @Test
  void getWorkArea() throws Exception {

    mockMvc.perform(
        get(ReverseRouter.route(on(WorkAreaController.class).getWorkArea())))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/workarea/workArea"))
        .andExpect(model().attribute("startScapUrl",
            ReverseRouter.route(on(ScapStartController.class).renderStartNewScap())));

  }

  @Test
  void teamMemberNotScapSubmitter_cannotStartScap() throws Exception {
    when(teamMemberService.listAllPermissionsForUserInAllTeams(any()))
        .thenReturn(List.of(RolePermission.VIEW_SCAP));

    mockMvc.perform(
            get(ReverseRouter.route(on(WorkAreaController.class).getWorkArea())))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/workarea/workArea"))
        .andExpect(model().attribute("startScapUrl",
            ReverseRouter.route(on(ScapStartController.class).renderStartNewScap())))
        .andExpect(model().attribute("canStartScap", false));
  }

  @Test
  void teamMemberScapSubmitter_canStartScap() throws Exception {
    when(teamMemberService.listAllPermissionsForUserInAllTeams(any()))
        .thenReturn(List.of(RolePermission.SUBMIT_SCAP));

    mockMvc.perform(
            get(ReverseRouter.route(on(WorkAreaController.class).getWorkArea())))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/workarea/workArea"))
        .andExpect(model().attribute("startScapUrl",
            ReverseRouter.route(on(ScapStartController.class).renderStartNewScap())))
        .andExpect(model().attribute("canStartScap", true));
  }
}
