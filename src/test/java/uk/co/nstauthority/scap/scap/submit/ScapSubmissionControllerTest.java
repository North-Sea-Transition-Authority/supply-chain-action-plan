package uk.co.nstauthority.scap.scap.submit;

import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.csrf;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.model;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.redirectedUrl;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.view;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.time.Instant;
import java.util.Collections;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ContextConfiguration;
import uk.co.nstauthority.scap.AbstractControllerTest;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.scap.Scap;
import uk.co.nstauthority.scap.scap.scap.ScapService;
import uk.co.nstauthority.scap.scap.submit.submissionviews.ProjectDetailsSubmissionView;
import uk.co.nstauthority.scap.scap.tasklist.TaskListController;
import uk.co.nstauthority.scap.workarea.WorkAreaController;

@ExtendWith(MockitoExtension.class)
@WithMockUser
@ContextConfiguration(classes = ScapSubmissionController.class)
class ScapSubmissionControllerTest extends AbstractControllerTest {

  private Integer scapId;
  private Scap scap;
  private ScapDetail scapDetail;

  @MockBean
  ScapService scapService;

  @MockBean
  ScapDetailService scapDetailService;

  @MockBean
  SubmissionViewService submissionViewService;

  @BeforeEach
  void setup() {
    scapId = 56;
    scap = new Scap(scapId);
    scap.setReference("SCAP/2022/1");
    scapDetail = new ScapDetail(scap, 1, true, ScapDetailStatus.DRAFT, Instant.now(), 1);
  }

  @Test
  void renderScapSubmissionConfirmation() throws Exception {
    var projectDetailsView = new ProjectDetailsSubmissionView(
        null, Collections.emptyList(), null, null, null,
        null, Collections.emptyList(), null, null
    );

    when(scapDetailService.getLatestScapDetailByScapIdOrThrow(scapId)).thenReturn(scapDetail);
    when(submissionViewService.getProjectDetailsSubmissionView(scapDetail)).thenReturn(projectDetailsView);

    mockMvc.perform(get(
        ReverseRouter.route(on(ScapSubmissionController.class).renderScapSubmissionConfirmation(scapId))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/submit/reviewAndSubmit"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(scapId))));
  }

  @Test
  void renderScapSubmissionConfirmation_AlreadySubmitted_AssertThrows() throws Exception {
    scapDetail.setStatus(ScapDetailStatus.SUBMITTED);
    when(scapDetailService.getLatestScapDetailByScapIdOrThrow(scapId)).thenReturn(scapDetail);

    mockMvc.perform(get(
            ReverseRouter.route(on(ScapSubmissionController.class).renderScapSubmissionConfirmation(scapId))))
        .andExpect(status().isBadRequest());
  }

  @Test
  void submitScap_VerifySubmits() throws Exception {
    var expectedRedirectUrl = ReverseRouter.route(on(ScapSubmissionController.class)
        .renderScapSubmissionSuccess(scapId));

    when(scapDetailService.getLatestScapDetailByScapIdOrThrow(scapId)).thenReturn(scapDetail);

    mockMvc.perform(post(
        ReverseRouter.route(on(ScapSubmissionController.class).submitScap(scapId)))
            .with(csrf()))
        .andExpect(status().is3xxRedirection())
        .andExpect(redirectedUrl(expectedRedirectUrl));

    verify(scapDetailService).submitScap(scapDetail);
  }

  @Test
  void submitScap_AlreadySubmitted_AssertThrows() throws Exception {
    scapDetail.setStatus(ScapDetailStatus.SUBMITTED);
    when(scapDetailService.getLatestScapDetailByScapIdOrThrow(scapId)).thenReturn(scapDetail);

    mockMvc.perform(post(
            ReverseRouter.route(on(ScapSubmissionController.class).submitScap(scapId)))
            .with(csrf()))
        .andExpect(status().isBadRequest());

    verify(scapDetailService, never()).submitScap(scapDetail);
  }

  @Test
  void renderScapSubmissionSuccess_NotSubmitted_AssertThrows() throws Exception {
    when(scapService.getScapById(scapId)).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);

    mockMvc.perform(get(
        ReverseRouter.route(on(ScapSubmissionController.class).renderScapSubmissionSuccess(scapId))))
        .andExpect(status().isBadRequest());
  }

  @Test
  void renderScapSubmissionSuccess() throws Exception {
    scapDetail.setStatus(ScapDetailStatus.SUBMITTED);
    when(scapService.getScapById(scapId)).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);

    mockMvc.perform(get(
        ReverseRouter.route(on(ScapSubmissionController.class).renderScapSubmissionSuccess(scapId))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/submit/submissionSuccess"))
        .andExpect(model().attribute("workAreaUrl",
            ReverseRouter.route(on(WorkAreaController.class).getWorkArea())))
        .andExpect(model().attribute("scapReference", scap.getReference()));
  }
}
