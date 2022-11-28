package uk.co.nstauthority.scap.scap.contractingperformance.summary;

import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.model;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.view;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.math.BigDecimal;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ContextConfiguration;
import uk.co.nstauthority.scap.AbstractControllerTest;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.RemunerationModel;
import uk.co.nstauthority.scap.scap.contractingperformance.hascontractingperformance.HasContractingPerformanceController;
import uk.co.nstauthority.scap.scap.tasklist.TaskListController;
import uk.co.nstauthority.scap.utils.ControllerTestingUtil;

@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes = ContractingPerformanceSummaryController.class)
@WithMockUser
class ContractingPerformanceSummaryControllerTest extends AbstractControllerTest {

  @MockBean
  ContractingPerformanceSummaryService contractingPerformanceSummaryService;

  private Integer scapId;

  @BeforeEach
  void setup() {
    scapId = 59;
  }

  @Test
  void renderContractingPerformanceSummary() throws Exception {
    var summaryViews = List.of(new ContractingPerformanceSummaryView(
        5106,
        "Some scope title",
        "Some scope description",
        BigDecimal.valueOf(1.2),
        RemunerationModel.LUMP_SUM,
        null,
        "Some contractor name",
        0,
        BigDecimal.valueOf(1.3),
        "Some outturn rationale"
    ));
    var countryMap = Map.of("0", "United Kingdom");

    when(contractingPerformanceSummaryService.getContractingPerformanceSummaryViews(scapId))
        .thenReturn(summaryViews);
    when(contractingPerformanceSummaryService.getCountryMap(summaryViews)).thenReturn(countryMap);

    mockMvc.perform(get(
        ReverseRouter.route(on(ContractingPerformanceSummaryController.class)
            .renderContractingPerformanceSummary(scapId))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/contractingperformance/contractingPerformanceSummary"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(scapId))))
        .andExpect(model().attribute("summaryViews", summaryViews))
        .andExpect(model().attribute("countryMap", countryMap));
  }

  @Test
  void renderContractingPerformanceSummary_NoContractingPerformances_AssertRedirects() throws Exception {
    var expectedRedirectUrl = ReverseRouter.route(on(HasContractingPerformanceController.class)
        .renderHasContractingPerformanceForm(scapId));

    when(contractingPerformanceSummaryService.getContractingPerformanceSummaryViews(scapId))
        .thenReturn(Collections.emptyList());

    mockMvc.perform(get(
            ReverseRouter.route(on(ContractingPerformanceSummaryController.class)
                .renderContractingPerformanceSummary(scapId))))
        .andExpect(status().is3xxRedirection())
        .andExpect(ControllerTestingUtil.redirectUrl(expectedRedirectUrl));
  }
}
