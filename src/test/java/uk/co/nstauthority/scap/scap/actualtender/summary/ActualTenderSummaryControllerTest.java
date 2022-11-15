package uk.co.nstauthority.scap.scap.actualtender.summary;

import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.model;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.view;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.math.BigDecimal;
import java.time.Instant;
import java.util.Collections;
import java.util.List;
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
import uk.co.nstauthority.scap.scap.actualtender.ActualTender;
import uk.co.nstauthority.scap.scap.actualtender.ActualTenderService;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivity;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivityService;
import uk.co.nstauthority.scap.scap.actualtender.activity.ContractStage;
import uk.co.nstauthority.scap.scap.actualtender.hasactualtender.HasActualTenderController;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.scap.Scap;
import uk.co.nstauthority.scap.scap.scap.ScapService;
import uk.co.nstauthority.scap.scap.tasklist.TaskListController;

@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes = ActualTenderSummaryController.class)
@WithMockUser
class ActualTenderSummaryControllerTest extends AbstractControllerTest {

  @MockBean
  ScapService scapService;

  @MockBean
  ScapDetailService scapDetailService;

  @MockBean
  ActualTenderService actualTenderService;

  @MockBean
  ActualTenderActivityService actualTenderActivityService;

  @MockBean
  ActualTenderSummaryService actualTenderSummaryService;

  private Scap scap;
  private ScapDetail scapDetail;
  private ActualTender actualTender;

  @BeforeEach
  void setup() {
    scap = new Scap(43);
    scapDetail = new ScapDetail(scap, 1, true, ScapDetailStatus.DRAFT, Instant.now(), 1);
    actualTender = new ActualTender(scapDetail, Instant.now());
  }

  @Test
  void renderActualTenderSummary_NoActualTenderActivities_AssertRedirects() throws Exception {
    var expectedRedirectUrl = ReverseRouter.route(on(HasActualTenderController.class)
        .renderHasActualTenderForm(scap.getId()));

    when(scapService.getScapById(scap.getId())).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(actualTenderService.getByScapDetailOrThrow(scapDetail)).thenReturn(actualTender);
    when(actualTenderActivityService.getAllByActualTender(actualTender)).thenReturn(Collections.emptyList());

    mockMvc.perform(get(
        ReverseRouter.route(on(ActualTenderSummaryController.class).renderActualTenderSummary(scap.getId()))))
        .andExpect(status().is3xxRedirection())
        .andExpect(view().name("redirect:%s".formatted(expectedRedirectUrl)));
  }

  @Test
  void renderActualTenderSummary() throws Exception {
    var actualTenderActivities = List.of(new ActualTenderActivity(430));
    var awardedContractSummaryView = new AwardedContractSummaryView(
        "preferred bidder name", BigDecimal.valueOf(1.32),
        "award rationale", "preferred bidder location");
    var actualTenderSummaryView = new ActualTenderSummaryView(
        "scope title", "scope description",
        RemunerationModel.OTHER, "remuneration model name",
        ContractStage.CONTRACT_AWARDED, List.of("ITT participant 1", "ITT participant 2"),
        List.of("bid participant 1", "bid participant 2"), awardedContractSummaryView,
        "changeLinkUrl", "deleteLinkUrl");
    var actualTenderActivitySummaryViews = List.of(actualTenderSummaryView);

    when(scapService.getScapById(scap.getId())).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(actualTenderService.getByScapDetailOrThrow(scapDetail)).thenReturn(actualTender);
    when(actualTenderActivityService.getAllByActualTender(actualTender)).thenReturn(actualTenderActivities);
    when(actualTenderSummaryService.getViewsForActualTenderActivities(actualTenderActivities, scap.getId()))
        .thenReturn(actualTenderActivitySummaryViews);

    mockMvc.perform(get(
        ReverseRouter.route(on(ActualTenderSummaryController.class).renderActualTenderSummary(scap.getId()))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/actualtender/actualTenderActivitySummary"))
        .andExpect(model().attribute("actualTenderActivities", actualTenderActivitySummaryViews))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(scap.getId()))));
  }
}
