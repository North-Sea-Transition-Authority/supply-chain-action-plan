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
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ContextConfiguration;
import uk.co.nstauthority.scap.AbstractControllerTest;
import uk.co.nstauthority.scap.authentication.ServiceUserDetail;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.start.ScapStartController;
import uk.co.nstauthority.scap.scap.summary.ScapSubmissionStage;

@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes = WorkAreaController.class)
@WithMockUser
class WorkAreaControllerTest extends AbstractControllerTest {

  @MockBean
  WorkAreaService workAreaService;

  @Test
  void getWorkArea() throws Exception {
    var workAreaItems = List.of(
        new WorkAreaItem(
            new ScapId(1),
            1,
            "ref",
            "operator",
            "projectName",
            ScapDetailStatus.DRAFT,
            ScapSubmissionStage.CONTRACTING_STRATEGY_PENDING,
            false
        ));

    when(workAreaService.getWorkAreaItems()).thenReturn(workAreaItems);

    mockMvc.perform(
        get(ReverseRouter.route(on(WorkAreaController.class).getWorkArea())))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/workarea/workArea"))
        .andExpect(model().attribute("startScapUrl",
            ReverseRouter.route(on(ScapStartController.class).renderStartNewScap())))
        .andExpect(model().attribute("workAreaItems", workAreaItems));

  }

  @Test
  void teamMemberNotScapSubmitter_cannotStartScap() throws Exception {
    when(teamMemberService.getAllPermissionsForUser(any(ServiceUserDetail.class)))
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
    when(teamMemberService.getAllPermissionsForUser(any(ServiceUserDetail.class)))
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
