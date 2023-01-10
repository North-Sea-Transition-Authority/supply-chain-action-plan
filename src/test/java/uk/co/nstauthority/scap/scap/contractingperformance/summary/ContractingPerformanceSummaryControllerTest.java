package uk.co.nstauthority.scap.scap.contractingperformance.summary;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
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
import static uk.co.nstauthority.scap.utils.ControllerTestingUtil.redirectUrl;

import java.math.BigDecimal;
import java.util.Collections;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.validation.BeanPropertyBindingResult;
import org.springframework.validation.BindingResult;
import org.springframework.validation.FieldError;
import uk.co.nstauthority.scap.AbstractScapSubmitterControllerTest;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.RemunerationModel;
import uk.co.nstauthority.scap.scap.contractingperformance.ContractingPerformanceController;
import uk.co.nstauthority.scap.scap.contractingperformance.ContractingPerformanceOverview;
import uk.co.nstauthority.scap.scap.contractingperformance.ContractingPerformanceOverviewService;
import uk.co.nstauthority.scap.scap.contractingperformance.hascontractingperformance.HasContractingPerformanceController;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.summary.contractingperformance.ContractingPerformanceSummaryView;
import uk.co.nstauthority.scap.scap.summary.contractingperformance.ContractingPerformanceSummaryViewService;
import uk.co.nstauthority.scap.scap.tasklist.TaskListController;

@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes = ContractingPerformanceSummaryController.class)
@WithMockUser
class ContractingPerformanceSummaryControllerTest extends AbstractScapSubmitterControllerTest {

  @MockBean
  ScapDetailService scapDetailService;

  @MockBean
  ContractingPerformanceOverviewService contractingPerformanceOverviewService;

  @MockBean
  HasMoreContractingPerformanceFormService hasMoreContractingPerformanceFormService;

  @MockBean
  ContractingPerformanceSummaryViewService contractingPerformanceSummaryViewService;

  private ScapDetail scapDetail;
  private ContractingPerformanceOverview contractingPerformanceOverview;
  private List<ContractingPerformanceSummaryView> summaryViews;

  @BeforeEach
  void setup() {
    scapDetail = new ScapDetail();
    contractingPerformanceOverview = new ContractingPerformanceOverview();
    summaryViews = List.of(new ContractingPerformanceSummaryView(
        SCAP_ID,
        5106,
        "Some scope title",
        "Some scope description",
        BigDecimal.valueOf(1.2),
        RemunerationModel.LUMP_SUM,
        null,
        "Some contractor name",
        "Untied Kingdom",
        BigDecimal.valueOf(1.3),
        "Some outturn rationale"
    ));
  }

  @Test
  void renderContractingPerformanceSummary() throws Exception {
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(contractingPerformanceOverviewService.getByScapDetailOrThrow(scapDetail))
        .thenReturn(contractingPerformanceOverview);
    when(contractingPerformanceSummaryViewService.getContractingPerformanceSummaryViews(SCAP_ID))
        .thenReturn(summaryViews);
    when(hasMoreContractingPerformanceFormService.getForm(contractingPerformanceOverview))
        .thenReturn(new HasMoreContractingPerformanceForm());

    mockMvc.perform(get(
        ReverseRouter.route(on(ContractingPerformanceSummaryController.class)
            .renderContractingPerformanceSummary(SCAP_ID))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/contractingperformance/contractingPerformanceSummary"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(SCAP_ID.scapId()))))
        .andExpect(model().attribute("summaryViews", summaryViews));
  }

  @Test
  void renderContractingPerformanceSummary_NoContractingPerformances_AssertRedirects() throws Exception {
    var expectedRedirectUrl = ReverseRouter.route(on(HasContractingPerformanceController.class)
        .renderHasContractingPerformanceForm(SCAP_ID));

    when(contractingPerformanceSummaryViewService.getContractingPerformanceSummaryViews(SCAP_ID))
        .thenReturn(Collections.emptyList());

    mockMvc.perform(get(
            ReverseRouter.route(on(ContractingPerformanceSummaryController.class)
                .renderContractingPerformanceSummary(SCAP_ID))))
        .andExpect(status().is3xxRedirection())
        .andExpect(redirectUrl(expectedRedirectUrl));
  }

  @Test
  void saveContractingPerformanceSummary_NoContractingPerformances_AssertRedirects() throws Exception {
    var expectedRedirectUrl = ReverseRouter.route(on(HasContractingPerformanceController.class)
        .renderHasContractingPerformanceForm(SCAP_ID));

    when(contractingPerformanceSummaryViewService.getContractingPerformanceSummaryViews(SCAP_ID))
        .thenReturn(Collections.emptyList());

    mockMvc.perform(post(
        ReverseRouter.route(on(ContractingPerformanceSummaryController.class)
            .renderContractingPerformanceSummary(SCAP_ID)))
            .with(csrf()))
        .andExpect(status().is3xxRedirection())
        .andExpect(redirectUrl(expectedRedirectUrl));

    verify(contractingPerformanceOverviewService, never()).updateHasMoreContractingPerformance(any(), any());
  }

  @Test
  void saveContractingPerformanceSummary_HasErrors_VerifyNeverSaves() throws Exception {
    var form = new HasMoreContractingPerformanceForm();
    var bindingResultWithErrors = new BeanPropertyBindingResult(form, "form");
    bindingResultWithErrors.addError(new FieldError(
        "form", "testField", "test error message"
    ));

    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(contractingPerformanceOverviewService.getByScapDetailOrThrow(scapDetail))
        .thenReturn(contractingPerformanceOverview);
    when(contractingPerformanceSummaryViewService.getContractingPerformanceSummaryViews(SCAP_ID))
        .thenReturn(summaryViews);
    when(hasMoreContractingPerformanceFormService.validate(eq(form), any(BindingResult.class)))
        .thenReturn(bindingResultWithErrors);

    mockMvc.perform(post(
        ReverseRouter.route(on(ContractingPerformanceSummaryController.class)
            .renderContractingPerformanceSummary(SCAP_ID)))
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/contractingperformance/contractingPerformanceSummary"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(SCAP_ID.scapId()))))
        .andExpect(model().attribute("summaryViews", summaryViews))
        .andExpect(model().attributeExists("errorList"));

    verify(contractingPerformanceOverviewService, never()).updateHasMoreContractingPerformance(any(), any());
  }

  @Test
  void saveContractingPerformanceSummary_NoErrors_AddMoreNow_VerifyUpdateAndRedirect() throws Exception {
    var expectedRedirectUrl = ReverseRouter.route(on(ContractingPerformanceController.class)
        .renderNewContractingPerformanceForm(SCAP_ID, null));
    var hasMoreContractingPerformance = HasMoreContractingPerformance.YES_NOW;
    var form = new HasMoreContractingPerformanceForm();
    form.setHasMoreContractingPerformance(hasMoreContractingPerformance);
    var bindingResultWithoutErrors = new BeanPropertyBindingResult(form, "form");

    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(contractingPerformanceOverviewService.getByScapDetailOrThrow(scapDetail))
        .thenReturn(contractingPerformanceOverview);
    when(contractingPerformanceSummaryViewService.getContractingPerformanceSummaryViews(SCAP_ID))
        .thenReturn(summaryViews);
    when(hasMoreContractingPerformanceFormService.validate(eq(form), any(BindingResult.class)))
        .thenReturn(bindingResultWithoutErrors);

    mockMvc.perform(post(
        ReverseRouter.route(on(ContractingPerformanceSummaryController.class)
            .renderContractingPerformanceSummary(SCAP_ID)))
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().is3xxRedirection())
        .andExpect(redirectUrl(expectedRedirectUrl));

    verify(contractingPerformanceOverviewService, never()).updateHasMoreContractingPerformance(any(), any());
  }

  @Test
  void saveContractingPerformanceSummary_NoErrors_VerifyUpdateAndRedirect() throws Exception {
    var expectedRedirectUrl = ReverseRouter.route(on(TaskListController.class).renderTaskList(SCAP_ID.scapId()));
    var hasMoreContractingPerformance = HasMoreContractingPerformance.NO;
    var form = new HasMoreContractingPerformanceForm();
    form.setHasMoreContractingPerformance(hasMoreContractingPerformance);
    var bindingResultWithoutErrors = new BeanPropertyBindingResult(form, "form");

    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(contractingPerformanceOverviewService.getByScapDetailOrThrow(scapDetail))
        .thenReturn(contractingPerformanceOverview);
    when(contractingPerformanceSummaryViewService.getContractingPerformanceSummaryViews(SCAP_ID))
        .thenReturn(summaryViews);
    when(hasMoreContractingPerformanceFormService.validate(eq(form), any(BindingResult.class)))
        .thenReturn(bindingResultWithoutErrors);

    mockMvc.perform(post(
        ReverseRouter.route(on(ContractingPerformanceSummaryController.class)
            .renderContractingPerformanceSummary(SCAP_ID)))
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().is3xxRedirection())
        .andExpect(redirectUrl(expectedRedirectUrl));

    verify(contractingPerformanceOverviewService).updateHasMoreContractingPerformance(
        contractingPerformanceOverview, hasMoreContractingPerformance
    );
  }
}
