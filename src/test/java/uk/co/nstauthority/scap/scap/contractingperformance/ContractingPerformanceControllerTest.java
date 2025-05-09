package uk.co.nstauthority.scap.scap.contractingperformance;

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
import static uk.co.nstauthority.scap.mvc.ReverseRouter.emptyBindingResult;
import static uk.co.nstauthority.scap.utils.ControllerTestingUtil.redirectUrl;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
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
import uk.co.nstauthority.scap.scap.actualtender.ActualTender;
import uk.co.nstauthority.scap.scap.actualtender.ActualTenderService;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivity;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivityService;
import uk.co.nstauthority.scap.scap.contractingperformance.hascontractingperformance.HasContractingPerformanceController;
import uk.co.nstauthority.scap.scap.contractingperformance.summary.ContractingPerformanceSummaryController;

@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes = ContractingPerformanceController.class)
@WithMockUser
class ContractingPerformanceControllerTest extends AbstractScapSubmitterControllerTest {

  @MockBean
  ContractingPerformanceOverviewService contractingPerformanceOverviewService;

  @MockBean
  ContractingPerformanceService contractingPerformanceService;

  @MockBean
  ContractingPerformanceFormService contractingPerformanceFormService;

  @MockBean
  ActualTenderService actualTenderService;

  @MockBean
  ActualTenderActivityService actualTenderActivityService;
  private ActualTender actualTender;
  private ContractingPerformanceOverview contractingPerformanceOverview;

  @BeforeEach
  void setup() {
    actualTender = new ActualTender();
    contractingPerformanceOverview = new ContractingPerformanceOverview();
  }

  @Test
  void renderNewContractingPerformanceForm() throws Exception {
    var activity = new ActualTenderActivity(3447);
    var activities = List.of(activity);
    var scopeTitlesMap = Map.of(String.valueOf(activity.getId()), "Activity scope title");

    when(scapService.getScapById(SCAP_ID)).thenReturn(scap);
    when(actualTenderService.findByScapDetail(scapDetail)).thenReturn(Optional.of(actualTender));
    when(actualTenderActivityService.getActivitiesWithContractAwarded(actualTender)).thenReturn(activities);
    when(contractingPerformanceService.getActivitiesWithoutContractingPerformance(activities)).thenReturn(activities);
    when(contractingPerformanceFormService.getScopeTitlesMap(activities)).thenReturn(scopeTitlesMap);

    mockMvc.perform(get(
        ReverseRouter.route(on(ContractingPerformanceController.class)
            .renderNewContractingPerformanceForm(scap.getScapId(), null))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/contractingperformance/contractingPerformanceDetail"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(HasContractingPerformanceController.class)
                .renderHasContractingPerformanceForm(scap.getScapId()))))
        .andExpect(model().attribute("scopeTitlesMap", scopeTitlesMap))
        .andExpect(model().attributeExists("form"));
  }

  @Test
  void renderNewContractingPerformanceForm_NoActualTender() throws Exception {
    when(scapService.getScapById(SCAP_ID)).thenReturn(scap);
    when(actualTenderService.findByScapDetail(scapDetail)).thenReturn(Optional.empty());

    mockMvc.perform(get(
        ReverseRouter.route(on(ContractingPerformanceController.class)
            .renderNewContractingPerformanceForm(scap.getScapId(), null))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/contractingperformance/contractingPerformanceDetail"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(HasContractingPerformanceController.class)
                .renderHasContractingPerformanceForm(scap.getScapId()))))
        .andExpect(model().attribute("scopeTitlesMap", Collections.emptyMap()))
        .andExpect(model().attributeExists("form"));
  }

  @Test
  void renderNewContractingPerformanceForm_HasContractingPerformances() throws Exception {
    var activity = new ActualTenderActivity(3447);
    var activities = List.of(activity);
    var scopeTitlesMap = Map.of(String.valueOf(activity.getId()), "Activity scope title");

    when(scapService.getScapById(SCAP_ID)).thenReturn(scap);
    when(actualTenderService.findByScapDetail(scapDetail)).thenReturn(Optional.of(actualTender));
    when(actualTenderActivityService.getActivitiesWithContractAwarded(actualTender)).thenReturn(activities);
    when(contractingPerformanceService.getActivitiesWithoutContractingPerformance(activities)).thenReturn(activities);
    when(contractingPerformanceFormService.getScopeTitlesMap(activities)).thenReturn(scopeTitlesMap);
    when(contractingPerformanceOverviewService.getByScapDetail(scapDetail))
        .thenReturn(contractingPerformanceOverview);
    when(contractingPerformanceService.hasContractingPerformance(contractingPerformanceOverview))
        .thenReturn(true);

    mockMvc.perform(get(
            ReverseRouter.route(on(ContractingPerformanceController.class)
                .renderNewContractingPerformanceForm(scap.getScapId(), null))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/contractingperformance/contractingPerformanceDetail"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(ContractingPerformanceSummaryController.class)
                .renderContractingPerformanceSummary(scap.getScapId()))))
        .andExpect(model().attribute("scopeTitlesMap", scopeTitlesMap))
        .andExpect(model().attributeExists("form"));
  }

  @Test
  void saveNewContractingPerformanceForm_NoErrors_VerifySaves() throws Exception {
    var activity = new ActualTenderActivity(3447);
    var activities = List.of(activity);
    var scopeTitlesMap = Map.of(String.valueOf(activity.getId()), "Activity scope title");
    var form = new ContractingPerformanceForm();
    var bindingResultWithoutErrors = new BeanPropertyBindingResult(form, "form");
    var expectedRedirectUrl = ReverseRouter.route(on(ContractingPerformanceSummaryController.class)
        .renderContractingPerformanceSummary(scap.getScapId()));

    when(scapService.getScapById(SCAP_ID)).thenReturn(scap);
    when(actualTenderService.findByScapDetail(scapDetail)).thenReturn(Optional.of(actualTender));
    when(actualTenderActivityService.getActivitiesWithContractAwarded(actualTender)).thenReturn(activities);
    when(contractingPerformanceOverviewService.getByScapDetail(scapDetail))
        .thenReturn(contractingPerformanceOverview);
    when(contractingPerformanceFormService.getScopeTitlesMap(activities)).thenReturn(scopeTitlesMap);
    when(contractingPerformanceFormService.validate(eq(form), any(BindingResult.class), eq(activities)))
        .thenReturn(bindingResultWithoutErrors);

    mockMvc.perform(post(
        ReverseRouter.route(on(ContractingPerformanceController.class)
            .saveNewContractingPerformanceForm(scap.getScapId(), null, emptyBindingResult())))
        .with(csrf())
        .flashAttr("form", form))
        .andExpect(status().is3xxRedirection())
        .andExpect(redirectUrl(expectedRedirectUrl));

    verify(contractingPerformanceService).createContractingPerformance(contractingPerformanceOverview, form);
  }

  @Test
  void saveNewContractingPerformanceForm_HasErrors_VerifyNeverSaves() throws Exception {
    var activity = new ActualTenderActivity(3447);
    var activities = List.of(activity);
    var scopeTitlesMap = Map.of(String.valueOf(activity.getId()), "Activity scope title");
    var form = new ContractingPerformanceForm();
    var bindingResultWithErrors = new BeanPropertyBindingResult(form, "form");
    bindingResultWithErrors.addError(
        new FieldError("form", "testField", "test error message")
    );

    when(scapService.getScapById(SCAP_ID)).thenReturn(scap);
    when(actualTenderService.findByScapDetail(scapDetail)).thenReturn(Optional.of(actualTender));
    when(actualTenderActivityService.getActivitiesWithContractAwarded(actualTender)).thenReturn(activities);
    when(contractingPerformanceOverviewService.getByScapDetail(scapDetail))
        .thenReturn(contractingPerformanceOverview);
    when(contractingPerformanceService.getActivitiesWithoutContractingPerformance(activities)).thenReturn(activities);
    when(contractingPerformanceFormService.getScopeTitlesMap(activities)).thenReturn(scopeTitlesMap);
    when(contractingPerformanceFormService.validate(eq(form), any(BindingResult.class), eq(activities)))
        .thenReturn(bindingResultWithErrors);

    mockMvc.perform(post(
            ReverseRouter.route(on(ContractingPerformanceController.class)
                .saveNewContractingPerformanceForm(scap.getScapId(), null, emptyBindingResult())))
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/contractingperformance/contractingPerformanceDetail"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(HasContractingPerformanceController.class)
                .renderHasContractingPerformanceForm(scap.getScapId()))))
        .andExpect(model().attribute("scopeTitlesMap", scopeTitlesMap))
        .andExpect(model().attribute("form", form))
        .andExpect(model().attributeExists("errorList"));

    verify(contractingPerformanceService, never()).createContractingPerformance(any(), any());
  }

  @Test
  void saveNewContractingPerformanceForm_HasErrorsAndNoActualTender_VerifyNeverSaves() throws Exception {
    var form = new ContractingPerformanceForm();
    var bindingResultWithErrors = new BeanPropertyBindingResult(form, "form");
    bindingResultWithErrors.addError(
        new FieldError("form", "testField", "test error message")
    );

    when(scapService.getScapById(SCAP_ID)).thenReturn(scap);
    when(actualTenderService.findByScapDetail(scapDetail)).thenReturn(Optional.empty());
    when(contractingPerformanceOverviewService.getByScapDetail(scapDetail))
        .thenReturn(contractingPerformanceOverview);
    when(contractingPerformanceFormService.validate(eq(form), any(BindingResult.class), eq(Collections.emptyList())))
        .thenReturn(bindingResultWithErrors);

    mockMvc.perform(post(
        ReverseRouter.route(on(ContractingPerformanceController.class)
            .saveNewContractingPerformanceForm(scap.getScapId(), null, emptyBindingResult())))
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/contractingperformance/contractingPerformanceDetail"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(HasContractingPerformanceController.class)
                .renderHasContractingPerformanceForm(scap.getScapId()))))
        .andExpect(model().attribute("scopeTitlesMap", Collections.emptyMap()))
        .andExpect(model().attribute("form", form))
        .andExpect(model().attributeExists("errorList"));

    verify(contractingPerformanceService, never()).createContractingPerformance(any(), any());
  }

  @Test
  void renderExistingContractingPerformanceForm() throws Exception {
    var contractingPerformanceId = 52;
    var contractingPerformance = new ContractingPerformance(contractingPerformanceId);
    var form = new ContractingPerformanceForm();
    var activity = new ActualTenderActivity(3447);
    var activities = List.of(activity);
    var scopeTitlesMap = Map.of(String.valueOf(activity.getId()), "Activity scope title");

    when(scapService.getScapById(SCAP_ID)).thenReturn(scap);
    when(actualTenderService.findByScapDetail(scapDetail)).thenReturn(Optional.of(actualTender));
    when(actualTenderActivityService.getActivitiesWithContractAwarded(actualTender)).thenReturn(activities);
    when(contractingPerformanceService.getById(contractingPerformanceId)).thenReturn(contractingPerformance);
    when(contractingPerformanceService.getActivitiesWithoutContractingPerformancesWithCurrent(activities, contractingPerformance))
        .thenReturn(activities);
    when(contractingPerformanceFormService.getForm(contractingPerformance)).thenReturn(form);
    when(contractingPerformanceFormService.getScopeTitlesMap(activities)).thenReturn(scopeTitlesMap);

    mockMvc.perform(get(
        ReverseRouter.route(on(ContractingPerformanceController.class)
            .renderExistingContractingPerformanceForm(scap.getScapId(), contractingPerformanceId))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/contractingperformance/contractingPerformanceDetail"))
        .andExpect(model().attribute("backLinkUrl", ReverseRouter.route(on(ContractingPerformanceSummaryController.class)
            .renderContractingPerformanceSummary(scap.getScapId()))))
        .andExpect(model().attribute("scopeTitlesMap", scopeTitlesMap))
        .andExpect(model().attribute("form", form));
  }

  @Test
  void saveExistingContractingPerformanceForm_NoErrors_VerifySaves() throws Exception {
    var contractingPerformanceId = 52;
    var contractingPerformance = new ContractingPerformance(contractingPerformanceId);
    var activity = new ActualTenderActivity(3447);
    var activities = List.of(activity);
    var scopeTitlesMap = Map.of(String.valueOf(activity.getId()), "Activity scope title");
    var form = new ContractingPerformanceForm();
    var bindingResultWithoutErrors = new BeanPropertyBindingResult(form, "form");
    var expectedRedirectUrl = ReverseRouter.route(on(ContractingPerformanceSummaryController.class)
        .renderContractingPerformanceSummary(scap.getScapId()));

    when(scapService.getScapById(SCAP_ID)).thenReturn(scap);
    when(actualTenderService.findByScapDetail(scapDetail)).thenReturn(Optional.of(actualTender));
    when(actualTenderActivityService.getActivitiesWithContractAwarded(actualTender)).thenReturn(activities);
    when(contractingPerformanceOverviewService.getByScapDetail(scapDetail))
        .thenReturn(contractingPerformanceOverview);
    when(contractingPerformanceService.getById(contractingPerformanceId)).thenReturn(contractingPerformance);
    when(contractingPerformanceFormService.getScopeTitlesMap(activities)).thenReturn(scopeTitlesMap);
    when(contractingPerformanceFormService.validate(eq(form), any(BindingResult.class), eq(activities)))
        .thenReturn(bindingResultWithoutErrors);

    mockMvc.perform(post(
            ReverseRouter.route(on(ContractingPerformanceController.class)
                .saveExistingContractingPerformanceForm(scap.getScapId(), contractingPerformanceId, null, emptyBindingResult())))
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().is3xxRedirection())
        .andExpect(redirectUrl(expectedRedirectUrl));

    verify(contractingPerformanceService).saveContractingPerformance(contractingPerformance, form);
  }

  @Test
  void saveExistingContractingPerformanceForm_HasErrors_VerifyNeverSaves() throws Exception {
    var contractingPerformanceId = 52;
    var contractingPerformance = new ContractingPerformance(contractingPerformanceId);
    var activity = new ActualTenderActivity(3447);
    var activities = List.of(activity);
    var scopeTitlesMap = Map.of(String.valueOf(activity.getId()), "Activity scope title");
    var form = new ContractingPerformanceForm();
    var bindingResultWithErrors = new BeanPropertyBindingResult(form, "form");
    bindingResultWithErrors.addError(
        new FieldError("form", "testField", "test error message")
    );

    when(actualTenderService.findByScapDetail(scapDetail)).thenReturn(Optional.of(actualTender));
    when(actualTenderActivityService.getActivitiesWithContractAwarded(actualTender)).thenReturn(activities);
    when(contractingPerformanceOverviewService.getByScapDetail(scapDetail))
        .thenReturn(contractingPerformanceOverview);
    when(contractingPerformanceService.getById(contractingPerformanceId)).thenReturn(contractingPerformance);
    when(contractingPerformanceService.getActivitiesWithoutContractingPerformancesWithCurrent(activities, contractingPerformance))
        .thenReturn(activities);
    when(contractingPerformanceFormService.getScopeTitlesMap(activities)).thenReturn(scopeTitlesMap);
    when(contractingPerformanceFormService.validate(eq(form), any(BindingResult.class), eq(activities)))
        .thenReturn(bindingResultWithErrors);

    mockMvc.perform(post(
        ReverseRouter.route(on(ContractingPerformanceController.class)
            .saveExistingContractingPerformanceForm(SCAP_ID, contractingPerformanceId, null, emptyBindingResult())))
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/contractingperformance/contractingPerformanceDetail"))
        .andExpect(model().attribute("backLinkUrl", ReverseRouter.route(on(ContractingPerformanceSummaryController.class)
            .renderContractingPerformanceSummary(scap.getScapId()))))
        .andExpect(model().attribute("scopeTitlesMap", scopeTitlesMap))
        .andExpect(model().attribute("form", form))
        .andExpect(model().attributeExists("errorList"));

    verify(contractingPerformanceService, never()).saveContractingPerformance(any(), any());
  }
}
