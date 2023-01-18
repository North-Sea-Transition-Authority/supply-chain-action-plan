package uk.co.nstauthority.scap.scap.contractingperformance.delete;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.csrf;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.model;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.view;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.math.BigDecimal;
import java.util.Optional;
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
import uk.co.nstauthority.scap.scap.RemunerationModel;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivity;
import uk.co.nstauthority.scap.scap.contractingperformance.ContractingPerformance;
import uk.co.nstauthority.scap.scap.contractingperformance.ContractingPerformanceOverview;
import uk.co.nstauthority.scap.scap.contractingperformance.ContractingPerformanceOverviewService;
import uk.co.nstauthority.scap.scap.contractingperformance.ContractingPerformanceService;
import uk.co.nstauthority.scap.scap.contractingperformance.hascontractingperformance.HasContractingPerformanceController;
import uk.co.nstauthority.scap.scap.contractingperformance.summary.ContractingPerformanceSummaryController;
import uk.co.nstauthority.scap.scap.summary.contractingperformance.ContractingPerformanceSummaryView;
import uk.co.nstauthority.scap.scap.summary.contractingperformance.ContractingPerformanceSummaryViewService;
import uk.co.nstauthority.scap.utils.ControllerTestingUtil;

@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes = DeleteContractingPerformanceController.class)
@WithMockUser
class DeleteContractingPerformanceControllerTest extends AbstractScapSubmitterControllerTest {

  @MockBean
  ContractingPerformanceOverviewService contractingPerformanceOverviewService;

  @MockBean
  ContractingPerformanceService contractingPerformanceService;

  @MockBean
  ContractingPerformanceSummaryViewService contractingPerformanceSummaryViewService;

  private Integer contractingPerformanceId = 4357;

  @BeforeEach
  void setup() {
    contractingPerformanceId = 4357;
  }

  @Test
  void renderDeleteContractingPerformanceConfirmation_NoSummaryView_AssertNotFound() throws Exception{
    when(contractingPerformanceSummaryViewService.getContractingPerformanceSummaryView(SCAP_ID, contractingPerformanceId))
        .thenReturn(Optional.empty());

    mockMvc.perform(get(
        ReverseRouter.route(on(DeleteContractingPerformanceController.class)
            .renderDeleteContractingPerformanceConfirmation(SCAP_ID, contractingPerformanceId))))
        .andExpect(status().isNotFound());
  }

  @Test
  void renderDeleteContractingPerformanceConfirmation() throws Exception {
    var view = new ContractingPerformanceSummaryView(
        SCAP_ID, contractingPerformanceId, "Test scope title", "Test scope description",
        BigDecimal.valueOf(47.59), RemunerationModel.LUMP_SUM, null, "contractor name",
        "United Kingdom", BigDecimal.valueOf(48.29), "some outturn rationale"
    );

    when(contractingPerformanceSummaryViewService.getContractingPerformanceSummaryView(SCAP_ID, contractingPerformanceId))
        .thenReturn(Optional.of(view));

    mockMvc.perform(get(
        ReverseRouter.route(on(DeleteContractingPerformanceController.class)
            .renderDeleteContractingPerformanceConfirmation(SCAP_ID, contractingPerformanceId))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/contractingperformance/deleteContractingPerformance"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(ContractingPerformanceSummaryController.class).renderContractingPerformanceSummary(SCAP_ID))))
        .andExpect(model().attribute("summaryView", view));
  }

  @Test
  @DisplayName("Verify redirect to \"Has contracting performance?\" when deleting last contracting performance")
  void saveDeleteContractingPerformance_VerifyDeletes() throws Exception {
    var contractingPerformanceOverview = new ContractingPerformanceOverview();
    var actualTenderActivity = new ActualTenderActivity();
    actualTenderActivity.setScopeTitle("test scope title");
    var contractingPerformance = new ContractingPerformance(contractingPerformanceId);
    contractingPerformance.setActualTenderActivity(actualTenderActivity);
    var expectedRedirectUrl = ReverseRouter.route(on(HasContractingPerformanceController.class)
        .renderHasContractingPerformanceForm(SCAP_ID));

    when(contractingPerformanceOverviewService.getByScapDetailOrThrow(scapDetail))
        .thenReturn(contractingPerformanceOverview);
    when(contractingPerformanceService.getById(contractingPerformanceId)).thenReturn(contractingPerformance);
    when(contractingPerformanceService.hasContractingPerformance(contractingPerformanceOverview)).thenReturn(false);

    mockMvc.perform(post(
        ReverseRouter.route(on(DeleteContractingPerformanceController.class)
            .saveDeleteContractingPerformance(SCAP_ID, contractingPerformanceId, null)))
            .with(csrf()))
        .andExpect(status().is3xxRedirection())
        .andExpect(ControllerTestingUtil.redirectUrl(expectedRedirectUrl));

    verify(contractingPerformanceOverviewService).updateHasMoreContractingPerformance(contractingPerformanceOverview, null);
    verify(contractingPerformanceService).deleteContractingPerformance(contractingPerformance);
  }

  @Test
  void saveDeleteContractingPerformance_StillHasContractingPerformance_VerifyDeleteAndRedirect() throws Exception {
    var contractingPerformanceOverview = new ContractingPerformanceOverview();
    var actualTenderActivity = new ActualTenderActivity();
    actualTenderActivity.setScopeTitle("test scope title");
    var contractingPerformance = new ContractingPerformance(contractingPerformanceId);
    contractingPerformance.setActualTenderActivity(actualTenderActivity);
    var expectedRedirectUrl = ReverseRouter.route(on(ContractingPerformanceSummaryController.class)
        .renderContractingPerformanceSummary(SCAP_ID));

    when(contractingPerformanceOverviewService.getByScapDetailOrThrow(scapDetail))
        .thenReturn(contractingPerformanceOverview);
    when(contractingPerformanceService.getById(contractingPerformanceId)).thenReturn(contractingPerformance);
    when(contractingPerformanceService.hasContractingPerformance(contractingPerformanceOverview)).thenReturn(true);

    mockMvc.perform(post(
        ReverseRouter.route(on(DeleteContractingPerformanceController.class)
            .saveDeleteContractingPerformance(SCAP_ID, contractingPerformanceId, null)))
            .with(csrf()))
        .andExpect(status().is3xxRedirection())
        .andExpect(ControllerTestingUtil.redirectUrl(expectedRedirectUrl));

    verify(contractingPerformanceService).deleteContractingPerformance(contractingPerformance);
  }
}
