package uk.co.nstauthority.scap.scap.contractingperformance.hascontractingperformance;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
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
import static uk.co.nstauthority.scap.mvc.ReverseRouter.emptyBindingResult;

import java.util.List;
import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.validation.BeanPropertyBindingResult;
import org.springframework.validation.BindingResult;
import org.springframework.validation.FieldError;
import uk.co.nstauthority.scap.AbstractScapSubmitterControllerTest;
import uk.co.nstauthority.scap.enumutil.YesNo;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.contractingperformance.ContractingPerformanceController;
import uk.co.nstauthority.scap.scap.contractingperformance.ContractingPerformanceOverview;
import uk.co.nstauthority.scap.scap.contractingperformance.ContractingPerformanceOverviewService;
import uk.co.nstauthority.scap.scap.contractingperformance.ContractingPerformanceService;
import uk.co.nstauthority.scap.scap.contractingperformance.summary.ContractingPerformanceSummaryController;
import uk.co.nstauthority.scap.scap.summary.contractingperformance.ContractingPerformanceSummaryView;
import uk.co.nstauthority.scap.scap.summary.contractingperformance.ContractingPerformanceSummaryViewService;
import uk.co.nstauthority.scap.scap.tasklist.TaskListController;
import uk.co.nstauthority.scap.utils.ControllerTestingUtil;

@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes = HasContractingPerformanceController.class)
@WithMockUser
class HasContractingPerformanceControllerTest extends AbstractScapSubmitterControllerTest {

  @MockBean
  ContractingPerformanceOverviewService contractingPerformanceOverviewService;

  @MockBean
  ContractingPerformanceService contractingPerformanceService;

  @MockBean
  HasContractingPerformanceFormService contractingPerformanceFormService;

  @MockBean
  ContractingPerformanceSummaryViewService contractingPerformanceSummaryViewService;

  @Mock
  List<ContractingPerformanceSummaryView> summaryViews;

  @Test
  void renderHasContractingPerformanceForm_NoExistingContractPerformanceOverview() throws Exception {
    when(contractingPerformanceOverviewService.findByScapDetail(scapDetail)).thenReturn(Optional.empty());

    mockMvc.perform(get(
        ReverseRouter.route(on(HasContractingPerformanceController.class)
            .renderHasContractingPerformanceForm(SCAP_ID))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/contractingperformance/hasContractingPerformance"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(scap.getScapId()))))
        .andExpect(model().attribute("radioItems", YesNo.getRadioOptions()))
        .andExpect(model().attributeExists("form"));

    verify(contractingPerformanceSummaryViewService).getContractingPerformanceSummaryViews(SCAP_ID);
  }

  @Test
  void renderHasContractingPerformanceForm_ExistingContractPerformanceOverview() throws Exception {
    var existingContractPerformanceOverview = new ContractingPerformanceOverview();
    var filledForm = new HasContractingPerformanceForm();

    when(contractingPerformanceOverviewService.findByScapDetail(scapDetail))
        .thenReturn(Optional.of(existingContractPerformanceOverview));
    when(contractingPerformanceFormService.getForm(existingContractPerformanceOverview)).thenReturn(filledForm);

    mockMvc.perform(get(
        ReverseRouter.route(on(HasContractingPerformanceController.class)
            .renderHasContractingPerformanceForm(SCAP_ID))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/contractingperformance/hasContractingPerformance"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(scap.getScapId()))))
        .andExpect(model().attribute("radioItems", YesNo.getRadioOptions()))
        .andExpect(model().attribute("form", filledForm));

    verify(contractingPerformanceSummaryViewService).getContractingPerformanceSummaryViews(SCAP_ID);
  }

  @Test
  void renderHasContractingPerformanceForm_HasContractingPerformances_AssertRedirects() throws Exception {
    var existingContractPerformanceOverview = new ContractingPerformanceOverview();
    var filledForm = new HasContractingPerformanceForm();
    var expectedRedirectUrl = ReverseRouter.route(on(ContractingPerformanceSummaryController.class)
        .renderContractingPerformanceSummary(SCAP_ID));

    when(contractingPerformanceOverviewService.findByScapDetail(scapDetail))
        .thenReturn(Optional.of(existingContractPerformanceOverview));
    when(contractingPerformanceSummaryViewService.getContractingPerformanceSummaryViews(SCAP_ID)).thenReturn(summaryViews);
    doReturn(false).when(summaryViews).isEmpty();
    when(contractingPerformanceFormService.getForm(existingContractPerformanceOverview)).thenReturn(filledForm);

    mockMvc.perform(get(
        ReverseRouter.route(on(HasContractingPerformanceController.class)
            .renderHasContractingPerformanceForm(SCAP_ID))))
        .andExpect(status().is3xxRedirection())
        .andExpect(ControllerTestingUtil.redirectUrl(expectedRedirectUrl));
  }

  @Test
  void saveHasContractingPerformanceForm_HasContractingPerformances_AssertRedirects() throws Exception {
    var existingContractPerformanceOverview = new ContractingPerformanceOverview();
    var expectedRedirectUrl = ReverseRouter.route(on(ContractingPerformanceSummaryController.class)
        .renderContractingPerformanceSummary(SCAP_ID));

    when(contractingPerformanceOverviewService.findByScapDetail(scapDetail))
        .thenReturn(Optional.of(existingContractPerformanceOverview));
    when(contractingPerformanceSummaryViewService.getContractingPerformanceSummaryViews(SCAP_ID)).thenReturn(summaryViews);
    doReturn(false).when(summaryViews).isEmpty();

    mockMvc.perform(post(
        ReverseRouter.route(on(HasContractingPerformanceController.class)
            .saveHasContractingPerformanceForm(SCAP_ID, null, emptyBindingResult())))
        .with(csrf()))
        .andExpect(status().is3xxRedirection())
        .andExpect(ControllerTestingUtil.redirectUrl(expectedRedirectUrl));

    verify(contractingPerformanceSummaryViewService).getContractingPerformanceSummaryViews(SCAP_ID);
  }

  @Test
  void saveHasContractingPerformanceForm_HasErrors_VerifyNeverSaves() throws Exception {
    var form = new HasContractingPerformanceForm();
    var bindingResultWithErrors = new BeanPropertyBindingResult(form, "form");
    bindingResultWithErrors.addError(
        new FieldError("form", "testField", "Test error message")
    );

    when(contractingPerformanceFormService.validate(eq(form), any(BindingResult.class)))
        .thenReturn(bindingResultWithErrors);

    mockMvc.perform(post(
        ReverseRouter.route(on(HasContractingPerformanceController.class)
            .saveHasContractingPerformanceForm(SCAP_ID, null, emptyBindingResult())))
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/contractingperformance/hasContractingPerformance"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(scap.getScapId()))))
        .andExpect(model().attribute("radioItems", YesNo.getRadioOptions()))
        .andExpect(model().attribute("form", form));

    verify(contractingPerformanceOverviewService, never()).saveContractingPerformance(any(), any());
    verify(contractingPerformanceSummaryViewService).getContractingPerformanceSummaryViews(SCAP_ID);
  }

  @Test
  void saveHasContractingPerformanceForm_NoErrors_VerifySaves() throws Exception {
    var form = new HasContractingPerformanceForm();
    form.setHasContractingPerformance(YesNo.YES);
    var bindingResultWithoutErrors = new BeanPropertyBindingResult(form, "form");
    var expectedRedirectUrl = ReverseRouter.route(on(ContractingPerformanceController.class)
        .renderNewContractingPerformanceForm(SCAP_ID, null));

    when(contractingPerformanceFormService.validate(eq(form), any(BindingResult.class)))
        .thenReturn(bindingResultWithoutErrors);

    mockMvc.perform(post(
        ReverseRouter.route(on(HasContractingPerformanceController.class)
            .saveHasContractingPerformanceForm(SCAP_ID, null, emptyBindingResult())))
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().is3xxRedirection())
        .andExpect(ControllerTestingUtil.redirectUrl(expectedRedirectUrl));

    verify(contractingPerformanceOverviewService).saveContractingPerformance(scapDetail, form.getHasContractingPerformance());
    verify(contractingPerformanceSummaryViewService).getContractingPerformanceSummaryViews(SCAP_ID);
  }

  @Test
  void saveHasContractingPerformanceForm_NoErrors_NoContractingPerformance_VerifySaveAndRedirect() throws Exception {
    var form = new HasContractingPerformanceForm();
    form.setHasContractingPerformance(YesNo.NO);
    var bindingResultWithoutErrors = new BeanPropertyBindingResult(form, "form");
    var expectedRedirectUrl = ReverseRouter.route(on(TaskListController.class)
        .renderTaskList(scap.getScapId()));

    when(contractingPerformanceFormService.validate(eq(form), any(BindingResult.class)))
        .thenReturn(bindingResultWithoutErrors);

    mockMvc.perform(post(
        ReverseRouter.route(on(HasContractingPerformanceController.class)
            .saveHasContractingPerformanceForm(SCAP_ID, null, emptyBindingResult())))
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().is3xxRedirection())
        .andExpect(ControllerTestingUtil.redirectUrl(expectedRedirectUrl));

    verify(contractingPerformanceOverviewService).saveContractingPerformance(scapDetail, form.getHasContractingPerformance());
    verify(contractingPerformanceSummaryViewService).getContractingPerformanceSummaryViews(SCAP_ID);
  }
}
