package uk.co.nstauthority.scap.scap.casemanagement.update;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.csrf;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;
import static uk.co.nstauthority.scap.authentication.TestUserProvider.user;
import static uk.co.nstauthority.scap.utils.ControllerTestingUtil.redirectUrl;

import java.util.List;
import java.util.Optional;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ContextConfiguration;
import uk.co.nstauthority.scap.AbstractControllerTest;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventAction;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.scap.Scap;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.tasklist.TaskListController;

@ExtendWith(MockitoExtension.class)
@WithMockUser
@ContextConfiguration(classes = ScapUpdateController.class)
class ScapUpdateControllerTest extends AbstractControllerTest {

  private static final ScapId SCAP_ID = new ScapId(1000);

  private ScapDetail scapDetail;

  private Scap scap;

  @BeforeEach
  void setup() {
    scap = new Scap(SCAP_ID);
    scapDetail = new ScapDetail();
    when(scapService.getScapById(SCAP_ID.scapId())).thenReturn(scap);
    when(userDetailService.getUserDetail()).thenReturn(testUser);
    when(teamMemberService.getAllPermissionsForUser(testUser)).thenReturn(List.of(RolePermission.values()));
  }

  @Test
  void updateScap_noUpdateInProgress_newDraft() throws Exception {
    scapDetail.setStatus(ScapDetailStatus.SUBMITTED);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(scapDetailService.findLatestByScapIdAndStatus(SCAP_ID, ScapDetailStatus.DRAFT)).thenReturn(Optional.empty());

    mockMvc.perform(post(ReverseRouter.route(on(ScapUpdateController.class)
        .startScapUpdate(
            SCAP_ID,
            CaseEventAction.UPDATE_SUBMITTED)))
        .with(user(testUser))
        .with(csrf()))
        .andExpect(redirectUrl(ReverseRouter.route(on(TaskListController.class).renderTaskList(SCAP_ID))));
    verify(scapDetailService).createDraftScapDetail(scapService.getScapById(SCAP_ID));
  }

  @ParameterizedTest
  @EnumSource(
      value = ScapDetailStatus.class,
      names = {"DRAFT", "SUBMITTED", "APPROVED"},
      mode = EnumSource.Mode.EXCLUDE)
  void updateScap_NotInState_Throws(ScapDetailStatus status) throws Exception {
    scapDetail.setStatus(status);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);

    mockMvc.perform(post(ReverseRouter.route(on(ScapUpdateController.class)
            .startScapUpdate(
                SCAP_ID,
                CaseEventAction.UPDATE_SUBMITTED)))
            .with(user(testUser))
            .with(csrf()))
        .andExpect(status().is4xxClientError());
    verify(scapDetailService, never()).createDraftScapDetail(scapService.getScapById(SCAP_ID));
  }

  @Test
  void updateScap_UpdateInProgress_skipsNewDraft() throws Exception {
    scapDetail.setStatus(ScapDetailStatus.SUBMITTED);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(any(Scap.class))).thenReturn(scapDetail);
    when(scapDetailService.findLatestByScapIdAndStatus(SCAP_ID, ScapDetailStatus.DRAFT)).thenReturn(Optional.of(new ScapDetail()));

    mockMvc.perform(post(ReverseRouter.route(on(ScapUpdateController.class)
            .startScapUpdate(
                SCAP_ID,
                CaseEventAction.UPDATE_SUBMITTED)))
            .with(user(testUser))
            .with(csrf()))
        .andExpect(redirectUrl(ReverseRouter.route(on(TaskListController.class).renderTaskList(SCAP_ID))));
    verify(scapDetailService, never()).createDraftScapDetail(scapService.getScapById(SCAP_ID));
  }
}
