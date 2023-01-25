package uk.co.nstauthority.scap.scap.delete;

import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.verify;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.csrf;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.model;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.redirectedUrl;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.view;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;
import static uk.co.nstauthority.scap.scap.summary.ScapSummaryControllerTestUtil.getScapSummaryView;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ContextConfiguration;
import uk.co.nstauthority.scap.AbstractScapSubmitterControllerTest;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.summary.ScapSummaryViewService;
import uk.co.nstauthority.scap.scap.tasklist.TaskListController;
import uk.co.nstauthority.scap.workarea.WorkAreaController;

@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes = ScapDeletionController.class)
@WithMockUser
class ScapDeletionControllerTest extends AbstractScapSubmitterControllerTest {

  @MockBean
  ScapSummaryViewService scapSummaryViewService;

  @Test
  void renderScapDeletionConfirmation() throws Exception {
    doReturn(getScapSummaryView()).when(scapSummaryViewService).getScapSummaryView(scapDetail);

    mockMvc.perform(
        get(ReverseRouter.route(on(ScapDeletionController.class).renderScapDeletionConfirmation(SCAP_ID))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/deleteScap"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(SCAP_ID))))
        .andExpect(model().attribute("scapSummaryView", getScapSummaryView()))
        .andExpect(model().attribute("reference", scap.getReference()));
  }

  @Test
  void deleteScap() throws Exception {
    mockMvc.perform(
        post(ReverseRouter.route(on(ScapDeletionController.class).deleteScap(SCAP_ID, null)))
            .with(csrf()))
        .andExpect(status().is3xxRedirection())
        .andExpect(redirectedUrl(ReverseRouter.route(on(WorkAreaController.class).getWorkArea())));

    verify(scapDetailService).deleteScapById(SCAP_ID);
  }
}
