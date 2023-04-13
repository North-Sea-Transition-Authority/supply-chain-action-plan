package uk.co.nstauthority.scap.scap.actualtender.activity.delete;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.csrf;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.model;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.view;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.Collections;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ContextConfiguration;
import uk.co.nstauthority.scap.AbstractScapSubmitterControllerTest;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.RemunerationModel;
import uk.co.nstauthority.scap.scap.actualtender.ActualTender;
import uk.co.nstauthority.scap.scap.actualtender.ActualTenderControllerRedirectionServiceTestConfig;
import uk.co.nstauthority.scap.scap.actualtender.ActualTenderService;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivity;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivityService;
import uk.co.nstauthority.scap.scap.actualtender.activity.ContractStage;
import uk.co.nstauthority.scap.scap.actualtender.hasactualtender.HasActualTenderController;
import uk.co.nstauthority.scap.scap.actualtender.summary.ActualTenderSummaryController;
import uk.co.nstauthority.scap.scap.contractingperformance.ContractingPerformanceService;
import uk.co.nstauthority.scap.scap.summary.actualtender.ActualTenderActivitySummaryView;
import uk.co.nstauthority.scap.scap.summary.actualtender.ActualTenderSummaryViewService;
import uk.co.nstauthority.scap.utils.ControllerTestingUtil;

@ExtendWith(MockitoExtension.class)
@WithMockUser
@ContextConfiguration(classes = DeleteActualTenderActivityController.class)
@Import(ActualTenderControllerRedirectionServiceTestConfig.class)
class DeleteActualTenderActivityControllerTest extends AbstractScapSubmitterControllerTest {

  @MockBean
  ActualTenderService actualTenderService;

  @MockBean
  ActualTenderActivityService actualTenderActivityService;

  @MockBean
  ActualTenderSummaryViewService actualTenderSummaryViewService;

  @MockBean
  DeleteActualTenderActivityService deleteActualTenderActivityService;

  @MockBean
  ContractingPerformanceService contractingPerformanceService;

  private ActualTenderActivity actualTenderActivity;

  @BeforeEach
  void setup() {
    actualTenderActivity = new ActualTenderActivity(440);
  }

  @Test
  void renderDeleteActualTenderActivityConfirmation() throws Exception {
    var actualTenderSummaryView = new ActualTenderActivitySummaryView(
        scap.getScapId(),
        actualTenderActivity.getId(),
        "test scope title",
        "test scope description",
        RemunerationModel.LUMP_SUM,
        null,
        ContractStage.INVITATION_TO_TENDER_IS_LIVE,
        Map.of("company name", false),
        Collections.emptyMap(),
        null,
        true
    );

    when(scapService.getScapById(scap.getId())).thenReturn(scap);
    when(actualTenderActivityService.getById(actualTenderActivity.getId())).thenReturn(actualTenderActivity);
    when(actualTenderSummaryViewService.getSingleViewByActualTenderActivity(actualTenderActivity, scap.getScapId()))
        .thenReturn(actualTenderSummaryView);

    mockMvc.perform(get(
        ReverseRouter.route(on(DeleteActualTenderActivityController.class)
            .renderDeleteActualTenderActivityConfirmation(scap.getScapId(), actualTenderActivity.getId()))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/actualtender/deleteActualTender"))
        .andExpect(model().attribute("backLinkUrl", ReverseRouter.route(on(ActualTenderSummaryController.class)
            .renderActualTenderSummary(scap.getScapId()))))
        .andExpect(model().attribute("actualTenderActivityView", actualTenderSummaryView));
  }

  @Test
  void renderDeleteActualTenderActivityConfirmation_HasContractingPerformance() throws Exception {
    var actualTenderSummaryView = new ActualTenderActivitySummaryView(
        scap.getScapId(),
        actualTenderActivity.getId(),
        "test scope title",
        "test scope description",
        RemunerationModel.LUMP_SUM,
        null,
        ContractStage.INVITATION_TO_TENDER_IS_LIVE,
        Map.of("company name", false),
        Collections.emptyMap(),
        null,
        true
    );

    when(scapService.getScapById(scap.getId())).thenReturn(scap);
    when(actualTenderActivityService.getById(actualTenderActivity.getId())).thenReturn(actualTenderActivity);
    when(actualTenderSummaryViewService.getSingleViewByActualTenderActivity(actualTenderActivity, scap.getScapId()))
        .thenReturn(actualTenderSummaryView);
    when(contractingPerformanceService.hasContractingPerformance(actualTenderActivity)).thenReturn(true);

    mockMvc.perform(get(
        ReverseRouter.route(on(DeleteActualTenderActivityController.class)
            .renderDeleteActualTenderActivityConfirmation(scap.getScapId(), actualTenderActivity.getId()))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/actualtender/deleteActualTender"))
        .andExpect(model().attribute("backLinkUrl", ReverseRouter.route(on(ActualTenderSummaryController.class)
            .renderActualTenderSummary(scap.getScapId()))))
        .andExpect(model().attribute("actualTenderActivityView", actualTenderSummaryView))
        .andExpect(model().attribute("contractingPerformanceWarning",
            DeleteActualTenderActivityController.DELETES_CONTRACTING_PERFORMANCE_WARNING));
  }

  @Test
  void submitDeleteActualTenderActivity_SomeRemainingActivities_VerifyDeleteAndRedirect() throws Exception {
    var expectedRedirectUrl = ReverseRouter.route(on(ActualTenderSummaryController.class)
        .renderActualTenderSummary(scap.getScapId()));
    var actualTender = new ActualTender();

    when(scapService.getScapById(scap.getId())).thenReturn(scap);
    when(actualTenderService.getByScapDetail(scapDetail)).thenReturn(actualTender);
    when(actualTenderActivityService.getById(actualTenderActivity.getId())).thenReturn(actualTenderActivity);
    when(actualTenderActivityService.hasActualTenderActivity(actualTender)).thenReturn(true);

    mockMvc.perform(post(
        ReverseRouter.route(on(DeleteActualTenderActivityController.class)
            .renderDeleteActualTenderActivityConfirmation(scap.getScapId(), actualTenderActivity.getId())))
            .with(csrf()))
        .andExpect(status().is3xxRedirection())
        .andExpect(ControllerTestingUtil.redirectUrl(expectedRedirectUrl));

    verify(deleteActualTenderActivityService).deleteActualTenderActivity(actualTenderActivity);
  }

  @Test
  void submitDeleteActualTenderActivity_NoRemainingActivities_VerifyDeleteAndRedirect() throws Exception {
    var expectedRedirectUrl = ReverseRouter.route(on(HasActualTenderController.class)
        .renderHasActualTenderForm(scap.getScapId()));
    var actualTender = new ActualTender();

    when(scapService.getScapById(scap.getId())).thenReturn(scap);
    when(actualTenderService.getByScapDetail(scapDetail)).thenReturn(actualTender);
    when(actualTenderActivityService.getById(actualTenderActivity.getId())).thenReturn(actualTenderActivity);
    when(actualTenderActivityService.hasActualTenderActivity(actualTender)).thenReturn(false);

    mockMvc.perform(post(
            ReverseRouter.route(on(DeleteActualTenderActivityController.class)
                .renderDeleteActualTenderActivityConfirmation(scap.getScapId(), actualTenderActivity.getId())))
            .with(csrf()))
        .andExpect(status().is3xxRedirection())
        .andExpect(ControllerTestingUtil.redirectUrl(expectedRedirectUrl));

    verify(deleteActualTenderActivityService).deleteActualTenderActivity(actualTenderActivity);
    verify(actualTenderService).updateHasMoreActualTenders(actualTender, null);
  }
}
