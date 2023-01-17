package uk.co.nstauthority.scap.scap.submit;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.csrf;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.model;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.redirectedUrl;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.view;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;
import static uk.co.nstauthority.scap.scap.summary.ScapSummaryControllerTestUtil.getScapSummaryView;
import static uk.co.nstauthority.scap.scap.summary.ScapSummaryControllerTestUtil.mockScapSummaryViewServiceMethods;

import java.time.Instant;
import java.util.List;
import java.util.stream.Stream;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ContextConfiguration;
import uk.co.nstauthority.scap.AbstractScapSubmitterControllerTest;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.scap.ScapFormTaskListSection;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.summary.ScapSummaryViewService;
import uk.co.nstauthority.scap.scap.tasklist.ScapTaskListItem;
import uk.co.nstauthority.scap.scap.tasklist.TaskListController;
import uk.co.nstauthority.scap.scap.timeline.TimelineEventService;
import uk.co.nstauthority.scap.scap.timeline.TimelineEventSubject;
import uk.co.nstauthority.scap.workarea.WorkAreaController;

@ExtendWith(MockitoExtension.class)
@WithMockUser
@ContextConfiguration(classes = ScapSubmissionController.class)
class ScapSubmissionControllerTest extends AbstractScapSubmitterControllerTest {

  @MockBean
  ScapSummaryViewService scapSummaryViewService;

  @MockBean
  TimelineEventService timelineEventService;

  @MockBean
  List<ScapTaskListItem> scapTaskListItems;

  @BeforeEach
  void setup() {
    scap.setReference("SCAP/2022/1");
    scapDetail = new ScapDetail(scap, 1, true, ScapDetailStatus.DRAFT, Instant.now(), 1);
  }

  @Test
  @DisplayName("Render SCAP submission confirmation when all sections complete")
  void renderScapSubmissionConfirmation_AllSectionsComplete_IsSubmittable() throws Exception {
    var taskListItem = mock(ScapTaskListItem.class);
    var submitTaskListItem = mock(ScapTaskListItem.class);

    doReturn(Stream.of(taskListItem, submitTaskListItem)).when(scapTaskListItems).stream();
    doReturn(ScapFormTaskListSection.class).when(taskListItem).getTaskListSection();
    doReturn(ReviewAndSubmitTaskListSection.class).when(submitTaskListItem).getTaskListSection();
    doReturn(true).when(taskListItem).isValid(SCAP_ID.scapId());
    when(scapDetailService.getLatestScapDetailByScapIdOrThrow(SCAP_ID)).thenReturn(scapDetail);
    mockScapSummaryViewServiceMethods(scapSummaryViewService, scapDetail);

    mockMvc.perform(get(
        ReverseRouter.route(on(ScapSubmissionController.class).renderScapSubmissionConfirmation(SCAP_ID))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/submit/reviewAndSubmit"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(SCAP_ID))))
        .andExpect(model().attribute("scapSummaryView", getScapSummaryView()))
        .andExpect(model().attribute("isValid", true));

    verify(submitTaskListItem, never()).isValid(any());
  }

  @Test
  @DisplayName("Render SCAP submission confirmation when not complete")
  void renderScapSubmissionConfirmation_NotComplete() throws Exception {
    var taskListItem = mock(ScapTaskListItem.class);

    doReturn(Stream.of(taskListItem)).when(scapTaskListItems).stream();
    doReturn(ScapFormTaskListSection.class).when(taskListItem).getTaskListSection();
    doReturn(false).when(taskListItem).isValid(SCAP_ID.scapId());
    when(scapDetailService.getLatestScapDetailByScapIdOrThrow(SCAP_ID)).thenReturn(scapDetail);
    when(scapSummaryViewService.getScapSummaryView(scapDetail)).thenReturn(getScapSummaryView());

    mockMvc.perform(get(
            ReverseRouter.route(on(ScapSubmissionController.class).renderScapSubmissionConfirmation(SCAP_ID))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/submit/reviewAndSubmit"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(SCAP_ID))))
        .andExpect(model().attribute("scapSummaryView", getScapSummaryView()))
        .andExpect(model().attribute("isValid", false))
        .andExpect(model().attribute("incompleteErrorMessage", ScapSubmissionController.INVALID_SCAP_ERROR_MESSAGE));
  }

  @Test
  void submitScap_VerifySubmitsAndRecords() throws Exception {
    var expectedRedirectUrl = ReverseRouter.route(on(ScapSubmissionController.class)
        .renderScapSubmissionSuccess(SCAP_ID));

    when(scapDetailService.getLatestScapDetailByScapIdOrThrow(SCAP_ID)).thenReturn(scapDetail);

    mockMvc.perform(post(
        ReverseRouter.route(on(ScapSubmissionController.class).submitScap(SCAP_ID)))
            .with(csrf()))
        .andExpect(status().is3xxRedirection())
        .andExpect(redirectedUrl(expectedRedirectUrl));

    verify(scapDetailService).submitScap(scapDetail);
    verify(timelineEventService).recordNewEvent(eq(TimelineEventSubject.SCAP_SUBMITTED),
        any(ScapId.class),
        eq(scapDetail.getVersionNumber()));
  }

  @Test
  @DisplayName("Assert that submitting a SCAP which is not complete throws a 400")
  void submitScap_NotValid_AssertThrows() throws Exception {
    var taskListItem = mock(ScapTaskListItem.class);

    doReturn(Stream.of(taskListItem)).when(scapTaskListItems).stream();
    doReturn(ScapFormTaskListSection.class).when(taskListItem).getTaskListSection();
    doReturn(false).when(taskListItem).isValid(SCAP_ID.scapId());
    when(scapDetailService.getLatestScapDetailByScapIdOrThrow(SCAP_ID)).thenReturn(scapDetail);

    mockMvc.perform(post(
        ReverseRouter.route(on(ScapSubmissionController.class).submitScap(SCAP_ID)))
            .with(csrf()))
        .andExpect(status().isBadRequest());

    verify(scapDetailService, never()).submitScap(scapDetail);
    verifyNoInteractions(timelineEventService);
  }

  @Test
  void renderScapSubmissionSuccess_NotSubmitted_AssertThrows() throws Exception {
    mockMvc.perform(get(
        ReverseRouter.route(on(ScapSubmissionController.class).renderScapSubmissionSuccess(SCAP_ID))))
        .andExpect(status().isBadRequest());
  }

  @Test
  void renderScapSubmissionSuccess() throws Exception {
    scapDetail.setStatus(ScapDetailStatus.SUBMITTED);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);

    mockMvc.perform(get(
        ReverseRouter.route(on(ScapSubmissionController.class).renderScapSubmissionSuccess(SCAP_ID))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/submit/submissionSuccess"))
        .andExpect(model().attribute("workAreaUrl",
            ReverseRouter.route(on(WorkAreaController.class).getWorkArea())))
        .andExpect(model().attribute("scapReference", scap.getReference()));
  }
}
