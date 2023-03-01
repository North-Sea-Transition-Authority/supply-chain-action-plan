package uk.co.nstauthority.scap.scap.actualtender.summary;

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

import java.math.BigDecimal;
import java.time.Instant;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.validation.BeanPropertyBindingResult;
import org.springframework.validation.BindingResult;
import org.springframework.validation.FieldError;
import uk.co.nstauthority.scap.AbstractScapSubmitterControllerTest;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.RemunerationModel;
import uk.co.nstauthority.scap.scap.actualtender.ActualTender;
import uk.co.nstauthority.scap.scap.actualtender.ActualTenderControllerRedirectionServiceTestConfig;
import uk.co.nstauthority.scap.scap.actualtender.ActualTenderService;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivity;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivityController;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivityService;
import uk.co.nstauthority.scap.scap.actualtender.activity.ContractStage;
import uk.co.nstauthority.scap.scap.actualtender.hasactualtender.HasActualTenderController;
import uk.co.nstauthority.scap.scap.summary.actualtender.ActualTenderActivitySummaryView;
import uk.co.nstauthority.scap.scap.summary.actualtender.ActualTenderSummaryViewService;
import uk.co.nstauthority.scap.scap.summary.actualtender.AwardedContractSummaryView;
import uk.co.nstauthority.scap.scap.tasklist.TaskListController;
import uk.co.nstauthority.scap.utils.ControllerTestingUtil;

@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes = ActualTenderSummaryController.class)
@WithMockUser
@Import(ActualTenderControllerRedirectionServiceTestConfig.class)
class ActualTenderSummaryControllerTest extends AbstractScapSubmitterControllerTest {

  @MockBean
  ActualTenderService actualTenderService;

  @MockBean
  ActualTenderActivityService actualTenderActivityService;

  @MockBean
  ActualTenderSummaryViewService actualTenderSummaryViewService;

  @MockBean
  ActualTenderSummaryFormService actualTenderSummaryFormService;

  private ActualTender actualTender;

  @BeforeEach
  void setup() {
    actualTender = new ActualTender(scapDetail, Instant.now());
  }

  @Test
  void renderActualTenderSummary_NoActualTenderActivities_AssertRedirects() throws Exception {
    var expectedRedirectUrl = ReverseRouter.route(on(HasActualTenderController.class)
        .renderHasActualTenderForm(scap.getScapId()));

    when(scapService.getScapById(scap.getId())).thenReturn(scap);
    when(actualTenderService.getByScapDetailOrThrow(scapDetail)).thenReturn(actualTender);
    when(actualTenderActivityService.getAllByActualTender(actualTender)).thenReturn(Collections.emptyList());

    mockMvc.perform(get(
        ReverseRouter.route(on(ActualTenderSummaryController.class).renderActualTenderSummary(scap.getScapId()))))
        .andExpect(status().is3xxRedirection())
        .andExpect(view().name("redirect:%s".formatted(expectedRedirectUrl)));
  }

  @Test
  void renderActualTenderSummary() throws Exception {
    var actualTenderActivities = List.of(new ActualTenderActivity(430));
    var awardedContractSummaryView = new AwardedContractSummaryView(
        "preferred bidder name", BigDecimal.valueOf(1.32),
        "award rationale", "preferred bidder location",
        null, null, null, null);
    var actualTenderSummaryView = new ActualTenderActivitySummaryView(
        scap.getScapId(), actualTenderActivities.get(0).getId(),
        "scope title", "scope description",
        RemunerationModel.OTHER, "remuneration model name",
        ContractStage.CONTRACT_AWARDED,
        Map.of("ITT participant 1", false, "ITT participant 2", false),
        Map.of("bid participant 1", false, "bid participant 2", false), awardedContractSummaryView);
    var actualTenderActivitySummaryViews = List.of(actualTenderSummaryView);

    when(actualTenderService.getByScapDetailOrThrow(scapDetail)).thenReturn(actualTender);
    when(actualTenderActivityService.getAllByActualTender(actualTender)).thenReturn(actualTenderActivities);
    when(actualTenderSummaryViewService.getByActualTenderActivities(actualTenderActivities, scap.getScapId()))
        .thenReturn(actualTenderActivitySummaryViews);
    when(actualTenderSummaryFormService.getForm(actualTender)).thenReturn(new ActualTenderSummaryForm());

    mockMvc.perform(get(
        ReverseRouter.route(on(ActualTenderSummaryController.class).renderActualTenderSummary(scap.getScapId()))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/actualtender/actualTenderActivitySummary"))
        .andExpect(model().attribute("actualTenderActivities", actualTenderActivitySummaryViews))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(scap.getScapId()))));
  }

  @Test
  void saveActualTenderSummary_NoActualTenderActivities_AssertRedirects() throws Exception {
    var expectedRedirectUrl = ReverseRouter.route(on(HasActualTenderController.class)
        .renderHasActualTenderForm(scap.getScapId()));

    when(scapService.getScapById(scap.getId())).thenReturn(scap);
    when(actualTenderService.getByScapDetailOrThrow(scapDetail)).thenReturn(actualTender);
    when(actualTenderActivityService.getAllByActualTender(actualTender)).thenReturn(Collections.emptyList());

    mockMvc.perform(post(
        ReverseRouter.route(on(ActualTenderSummaryController.class)
            .saveActualTenderSummary(scap.getScapId(), null, emptyBindingResult())))
            .with(csrf()))
        .andExpect(status().is3xxRedirection())
        .andExpect(ControllerTestingUtil.redirectUrl(expectedRedirectUrl));
  }

  @Test
  void saveActualTenderSummary_HasErrors_VerifyNeverSaves() throws Exception {
    var actualTenderActivities = List.of(new ActualTenderActivity(430));
    var awardedContractSummaryView = new AwardedContractSummaryView(
        "preferred bidder name", BigDecimal.valueOf(1.32),
        "award rationale", "preferred bidder location", null,
        null, null, null);
    var actualTenderSummaryView = new ActualTenderActivitySummaryView(
        scap.getScapId(), actualTenderActivities.get(0).getId(),
        "scope title", "scope description",
        RemunerationModel.OTHER, "remuneration model name",
        ContractStage.CONTRACT_AWARDED,
        Map.of("ITT participant 1", false, "ITT participant 2", false),
        Map.of("bid participant 1", false, "bid participant 2", false), awardedContractSummaryView);
    var actualTenderActivitySummaryViews = List.of(actualTenderSummaryView);
    var form = new ActualTenderSummaryForm();
    var bindingResultWithErrors = new BeanPropertyBindingResult(form, "form");
    bindingResultWithErrors.addError(
        new FieldError("form", "testField", "Test error message")
    );

    when(actualTenderService.getByScapDetailOrThrow(scapDetail)).thenReturn(actualTender);
    when(actualTenderActivityService.getAllByActualTender(actualTender)).thenReturn(actualTenderActivities);
    when(actualTenderSummaryViewService.getByActualTenderActivities(actualTenderActivities, scap.getScapId()))
        .thenReturn(actualTenderActivitySummaryViews);
    when(actualTenderSummaryFormService.validate(eq(form), any(BindingResult.class)))
        .thenReturn(bindingResultWithErrors);

    mockMvc.perform(post(
        ReverseRouter.route(on(ActualTenderSummaryController.class)
            .saveActualTenderSummary(scap.getScapId(), null, emptyBindingResult())))
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/actualtender/actualTenderActivitySummary"))
        .andExpect(model().attribute("actualTenderActivities", actualTenderActivitySummaryViews))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(scap.getScapId()))))
        .andExpect(model().attributeExists("errorList"));

    verify(actualTenderService, never()).updateHasMoreActualTenders(any(), any());
  }

  @Test
  void saveActualTenderSummary_NoErrors_VerifySaves() throws Exception {
    var actualTenderActivities = List.of(new ActualTenderActivity(430));
    var awardedContractSummaryView = new AwardedContractSummaryView(
        "preferred bidder name", BigDecimal.valueOf(1.32),
        "award rationale", "preferred bidder location", null,
        null, null, null);
    var actualTenderSummaryView = new ActualTenderActivitySummaryView(
        scap.getScapId(), actualTenderActivities.get(0).getId(),
        "scope title", "scope description",
        RemunerationModel.OTHER, "remuneration model name",
        ContractStage.CONTRACT_AWARDED,
        Map.of("ITT participant 1", false, "ITT participant 2", false),
        Map.of("bid participant 1", false, "bid participant 2", false), awardedContractSummaryView);
    var actualTenderActivitySummaryViews = List.of(actualTenderSummaryView);
    var form = new ActualTenderSummaryForm();
    form.setHasMoreActualTenderActivities(HasMoreActualTenderActivities.YES_LATER);
    var bindingResultNoErrors = new BeanPropertyBindingResult(form, "form");
    var expectedRedirectUrl = ReverseRouter.route(on(TaskListController.class).renderTaskList(scap.getScapId()));

    when(actualTenderService.getByScapDetailOrThrow(scapDetail)).thenReturn(actualTender);
    when(actualTenderActivityService.getAllByActualTender(actualTender)).thenReturn(actualTenderActivities);
    when(actualTenderSummaryViewService.getByActualTenderActivities(actualTenderActivities, scap.getScapId()))
        .thenReturn(actualTenderActivitySummaryViews);
    when(actualTenderSummaryFormService.validate(eq(form), any(BindingResult.class)))
        .thenReturn(bindingResultNoErrors);

    mockMvc.perform(post(
        ReverseRouter.route(on(ActualTenderSummaryController.class)
            .saveActualTenderSummary(scap.getScapId(), null, emptyBindingResult())))
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().is3xxRedirection())
        .andExpect(ControllerTestingUtil.redirectUrl(expectedRedirectUrl));

    verify(actualTenderService).updateHasMoreActualTenders(actualTender, form.getHasMoreActualTenderActivities());
  }

  @Test
  void saveActualTenderSummary_NoErrors_AddAnother_AssertRedirect() throws Exception {
    var actualTenderActivities = List.of(new ActualTenderActivity(430));
    var awardedContractSummaryView = new AwardedContractSummaryView(
        "preferred bidder name", BigDecimal.valueOf(1.32),
        "award rationale", "preferred bidder location", null,
        null, null, null);
    var actualTenderSummaryView = new ActualTenderActivitySummaryView(
        scap.getScapId(), actualTenderActivities.get(0).getId(),
        "scope title", "scope description",
        RemunerationModel.OTHER, "remuneration model name",
        ContractStage.CONTRACT_AWARDED,
        Map.of("ITT participant 1", false, "ITT participant 2", false),
        Map.of("bid participant 1", false, "bid participant 2", false), awardedContractSummaryView);
    var actualTenderActivitySummaryViews = List.of(actualTenderSummaryView);
    var form = new ActualTenderSummaryForm();
    form.setHasMoreActualTenderActivities(HasMoreActualTenderActivities.YES_NOW);
    var bindingResultNoErrors = new BeanPropertyBindingResult(form, "form");
    var expectedRedirectUrl = ReverseRouter.route(on(ActualTenderActivityController.class)
        .renderActualTenderActivityForm(scap.getScapId(), null));

    when(actualTenderService.getByScapDetailOrThrow(scapDetail)).thenReturn(actualTender);
    when(actualTenderActivityService.getAllByActualTender(actualTender)).thenReturn(actualTenderActivities);
    when(actualTenderSummaryViewService.getByActualTenderActivities(actualTenderActivities, scap.getScapId()))
        .thenReturn(actualTenderActivitySummaryViews);
    when(actualTenderSummaryFormService.validate(eq(form), any(BindingResult.class)))
        .thenReturn(bindingResultNoErrors);

    mockMvc.perform(post(
        ReverseRouter.route(on(ActualTenderSummaryController.class)
            .saveActualTenderSummary(scap.getScapId(), null, emptyBindingResult())))
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().is3xxRedirection())
        .andExpect(ControllerTestingUtil.redirectUrl(expectedRedirectUrl));

    verify(actualTenderService).updateHasMoreActualTenders(actualTender, form.getHasMoreActualTenderActivities());
  }
}
