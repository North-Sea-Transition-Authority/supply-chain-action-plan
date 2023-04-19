package uk.co.nstauthority.scap.scap.submit;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
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
import static uk.co.nstauthority.scap.mvc.ReverseRouter.emptyBindingResult;
import static uk.co.nstauthority.scap.scap.summary.ScapSummaryControllerTestUtil.getScapSummaryView;
import static uk.co.nstauthority.scap.utils.ControllerTestingUtil.bindingResultWithErrors;

import java.time.Instant;
import java.time.LocalDate;
import java.util.Optional;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.validation.BindingResult;
import uk.co.nstauthority.scap.AbstractScapSubmitterControllerTest;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.notify.ScapEmailService;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEvent;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventService;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventSubject;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.summary.ScapSummaryViewService;
import uk.co.nstauthority.scap.scap.tasklist.TaskListController;
import uk.co.nstauthority.scap.workarea.WorkAreaController;
import uk.co.nstauthority.scap.workarea.updaterequests.UpdateRequest;
import uk.co.nstauthority.scap.workarea.updaterequests.UpdateRequestService;
import uk.co.nstauthority.scap.workarea.updaterequests.UpdateRequestType;

@ExtendWith(MockitoExtension.class)
@WithMockUser
@ContextConfiguration(classes = ScapSubmissionController.class)
class ScapSubmissionControllerTest extends AbstractScapSubmitterControllerTest {

  @MockBean
  ScapSummaryViewService scapSummaryViewService;

  @MockBean
  CaseEventService caseEventService;

  @MockBean
  ReviewAndSubmitFormService reviewAndSubmitFormService;

  @MockBean
  ScapEmailService scapEmailService;

  @MockBean
  UpdateRequestService updateRequestService;

  @MockBean
  ScapSubmissionService scapSubmissionService;

  @BeforeEach
  void setup() {
    scap.setReference("SCAP/2022/1");
    scapDetail = new ScapDetail(scap, 1, true, ScapDetailStatus.DRAFT, Instant.now(), 1);
  }

  @Test
  @DisplayName("Render SCAP submission confirmation when all sections complete")
  void renderScapSubmissionConfirmation_AllSectionsComplete_IsSubmittable() throws Exception {
    when(scapDetailService.getLatestScapDetailByScapIdOrThrow(SCAP_ID)).thenReturn(scapDetail);
    when(reviewAndSubmitFormService.getForm(scapDetail)).thenReturn(new ReviewAndSubmitForm());
    when(scapSummaryViewService.getScapSummaryView(scapDetail)).thenReturn(getScapSummaryView());
    when(scapSubmissionService.isScapValid(scapDetail)).thenReturn(true);
    when(updateRequestService.findNextDueUpdate(SCAP_ID)).thenReturn(Optional.of(getUpdateRequest()));

    mockMvc.perform(get(
        ReverseRouter.route(on(ScapSubmissionController.class).renderScapSubmissionConfirmation(SCAP_ID))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/submit/reviewAndSubmit"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(SCAP_ID))))
        .andExpect(model().attribute("scapSummaryView", getScapSummaryView()))
        .andExpect(model().attribute("isValid", true))
        .andExpect(model().attributeExists("form"))
        .andExpect(model().attribute("updateText", "TEST"));
  }

  @Test
  @DisplayName("Render SCAP submission confirmation when not complete")
  void renderScapSubmissionConfirmation_NotComplete() throws Exception {
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
    when(reviewAndSubmitFormService.validate(any(), any())).thenReturn(emptyBindingResult());
    when(scapSubmissionService.isScapValid(scapDetail)).thenReturn(true);

    mockMvc.perform(post(
        ReverseRouter.route(on(ScapSubmissionController.class).submitScap(SCAP_ID, null, emptyBindingResult())))
            .with(csrf()))
        .andExpect(status().is3xxRedirection())
        .andExpect(redirectedUrl(expectedRedirectUrl));

    verify(scapDetailService).submitScap(eq(scapDetail), any());
    verify(caseEventService).recordNewEvent(
        CaseEventSubject.SCAP_SUBMITTED,
        scapDetail,
        scapDetail.getVersionNumber(),
        null
    );
    verify(scapEmailService).sendScapSubmissionEmails(scapDetail);
  }

  @Test
  @DisplayName("Assert that submitting a SCAP without confirming that it has been reviewed is not submitted")
  void submitScap_NotReviewedInternally_AssertThrows() throws Exception {
    var form = new ReviewAndSubmitForm();

    when(scapDetailService.getLatestScapDetailByScapIdOrThrow(SCAP_ID)).thenReturn(scapDetail);
    when(reviewAndSubmitFormService.validate(eq(form), any(BindingResult.class)))
        .thenReturn(bindingResultWithErrors(form));
    when(scapSummaryViewService.getScapSummaryView(scapDetail)).thenReturn(getScapSummaryView());
    when(scapSubmissionService.isScapValid(scapDetail)).thenReturn(true);

    mockMvc.perform(post(
        ReverseRouter.route(on(ScapSubmissionController.class).submitScap(SCAP_ID, null, emptyBindingResult())))
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().isOk());

    verify(scapDetailService, never()).submitScap(any(), any());
    verifyNoInteractions(caseEventService);
  }

  @Test
  @DisplayName("Assert that submitting a SCAP which is not complete throws a 400")
  void submitScap_NotValid_AssertThrows() throws Exception {
    when(scapDetailService.getLatestScapDetailByScapIdOrThrow(SCAP_ID)).thenReturn(scapDetail);

    mockMvc.perform(post(
        ReverseRouter.route(on(ScapSubmissionController.class).submitScap(SCAP_ID, null, emptyBindingResult())))
            .with(csrf()))
        .andExpect(status().isBadRequest());
    verify(scapDetailService, never()).submitScap(eq(scapDetail), any());
    verifyNoInteractions(caseEventService);
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
            ReverseRouter.route(on(WorkAreaController.class).getWorkArea(null))))
        .andExpect(model().attribute("scapReference", scap.getReference()));
  }

  private UpdateRequest getUpdateRequest() {
    var caseEvent = new CaseEvent();
    caseEvent.setComments("TEST");

    return new UpdateRequest(
        scapDetail,
        UpdateRequestType.UPDATE,
        LocalDate.now(),
        caseEvent);
  }
}
