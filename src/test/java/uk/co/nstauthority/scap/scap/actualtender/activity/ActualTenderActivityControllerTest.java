package uk.co.nstauthority.scap.scap.actualtender.activity;

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

import java.time.Clock;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.validation.BeanPropertyBindingResult;
import org.springframework.validation.BindingResult;
import org.springframework.validation.FieldError;
import uk.co.nstauthority.scap.AbstractControllerTest;
import uk.co.nstauthority.scap.AbstractScapSubmitterControllerTest;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.RemunerationModel;
import uk.co.nstauthority.scap.scap.actualtender.ActualTender;
import uk.co.nstauthority.scap.scap.actualtender.ActualTenderControllerRedirectionServiceTestConfig;
import uk.co.nstauthority.scap.scap.actualtender.ActualTenderService;
import uk.co.nstauthority.scap.scap.actualtender.activity.bidparticipants.BidParticipantsController;
import uk.co.nstauthority.scap.scap.actualtender.hasactualtender.HasActualTenderController;
import uk.co.nstauthority.scap.scap.actualtender.summary.ActualTenderSummaryController;
import uk.co.nstauthority.scap.scap.contractingperformance.ContractingPerformanceService;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.scap.Scap;
import uk.co.nstauthority.scap.scap.scap.ScapService;
import uk.co.nstauthority.scap.utils.ControllerTestingUtil;

@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes = ActualTenderActivityController.class)
@WithMockUser
@Import(ActualTenderControllerRedirectionServiceTestConfig.class)
class ActualTenderActivityControllerTest extends AbstractScapSubmitterControllerTest {

  @Autowired
  Clock clock;

  @MockBean
  ScapDetailService scapDetailService;

  @MockBean
  ActualTenderService actualTenderService;

  @MockBean
  ActualTenderActivityFormService actualTenderActivityFormService;

  @MockBean
  ActualTenderActivityService actualTenderActivityService;

  @MockBean
  InvitationToTenderParticipantService invitationToTenderParticipantService;

  @MockBean
  ContractingPerformanceService contractingPerformanceService;

  @MockBean
  UpdateActualTenderActivityService updateActualTenderActivityService;

  private ScapDetail scapDetail;
  private ActualTender actualTender;

  @BeforeEach
  void setup() {
    scapDetail = new ScapDetail(scap, 1, true, ScapDetailStatus.DRAFT, clock.instant(), 1);
    actualTender = new ActualTender(scapDetail, clock.instant());
  }

  @Test
  void renderActualTenderActivityForm() throws Exception {
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(actualTenderService.getByScapDetailOrThrow(scapDetail)).thenReturn(actualTender);

    mockMvc.perform(
        get(ReverseRouter.route(on(ActualTenderActivityController.class)
            .renderActualTenderActivityForm(scap.getScapId(), null))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/actualtender/actualTenderActivityDetails"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(HasActualTenderController.class).renderHasActualTenderForm(scap.getScapId()))))
        .andExpect(model().attribute("remunerationModels", RemunerationModel.getRemunerationModels()))
        .andExpect(model().attribute("contractStages", ContractStage.getContractStages()))
        .andExpect(model().attribute("scopeTitleMaxLength",
            ActualTenderActivityFormValidator.MAX_SCOPE_TITLE_LENGTH.toString()));
  }

  @Test
  void saveActualTenderActivityForm_validForm_assertRedirects() throws Exception {
    var form = new ActualTenderActivityForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");
    var createdTenderActivity = new ActualTenderActivity(8735);
    createdTenderActivity.setContractStage(ContractStage.INVITATION_TO_TENDER);
    var expectedRedirectUrl = ReverseRouter.route(on(BidParticipantsController.class)
        .renderBidParticipantsForm(scap.getScapId(), createdTenderActivity.getId(), null));

    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(actualTenderService.getByScapDetailOrThrow(scapDetail)).thenReturn(actualTender);
    when(actualTenderActivityFormService.validate(eq(form), any(BindingResult.class), eq(actualTender)))
        .thenReturn(bindingResult);
    when(actualTenderActivityService.createActualTenderActivity(actualTender, form))
        .thenReturn(createdTenderActivity);

    mockMvc.perform(
        post(ReverseRouter.route(on(ActualTenderActivityController.class)
            .saveActualTenderActivityForm(scap.getScapId(), null, emptyBindingResult())))
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().is3xxRedirection())
        .andExpect(view().name(String.format("redirect:%s", expectedRedirectUrl)));

    verify(actualTenderActivityService).createActualTenderActivity(actualTender, form);
  }

  @Test
  void saveActualTenderActivityForm_validForm_requestForInformation_assertRedirects() throws Exception {
    var form = new ActualTenderActivityForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");
    var createdTenderActivity = new ActualTenderActivity(8735);
    createdTenderActivity.setContractStage(ContractStage.REQUEST_FOR_INFORMATION);
    var expectedRedirectUrl = ReverseRouter.route(on(ActualTenderSummaryController.class)
        .renderActualTenderSummary(scap.getScapId()));

    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(actualTenderService.getByScapDetailOrThrow(scapDetail)).thenReturn(actualTender);
    when(actualTenderActivityFormService.validate(eq(form), any(BindingResult.class), eq(actualTender)))
        .thenReturn(bindingResult);
    when(actualTenderActivityService.createActualTenderActivity(actualTender, form))
        .thenReturn(createdTenderActivity);

    mockMvc.perform(
        post(ReverseRouter.route(on(ActualTenderActivityController.class)
            .saveActualTenderActivityForm(scap.getScapId(), null, emptyBindingResult())))
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().is3xxRedirection())
        .andExpect(view().name(String.format("redirect:%s", expectedRedirectUrl)));

    verify(actualTenderActivityService).createActualTenderActivity(actualTender, form);
  }

  @Test
  void saveActualTenderActivityForm_invalidForm_assertOk() throws Exception {
    var form = new ActualTenderActivityForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");
    bindingResult.addError(new FieldError("form", "testField", "Test field must not be blank"));

    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(actualTenderService.getByScapDetailOrThrow(scapDetail)).thenReturn(actualTender);
    when(actualTenderActivityFormService.validate(eq(form), any(BindingResult.class), eq(actualTender)))
        .thenReturn(bindingResult);

    mockMvc.perform(
        post(ReverseRouter.route(on(ActualTenderActivityController.class)
            .saveActualTenderActivityForm(scap.getScapId(), null, emptyBindingResult())))
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/actualtender/actualTenderActivityDetails"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(HasActualTenderController.class).renderHasActualTenderForm(scap.getScapId()))))
        .andExpect(model().attribute("remunerationModels", RemunerationModel.getRemunerationModels()))
        .andExpect(model().attribute("contractStages", ContractStage.getContractStages()))
        .andExpect(model().attributeExists("errorList"))
        .andExpect(model().attribute("scopeTitleMaxLength",
            ActualTenderActivityFormValidator.MAX_SCOPE_TITLE_LENGTH.toString()));

    verify(actualTenderActivityService, never()).createActualTenderActivity(any(), any());
  }

  @Test
  void renderExistingActualTenderActivityForm_ExpectOk() throws Exception {
    var actualTenderActivity = new ActualTenderActivity(45);
    var participant = new InvitationToTenderParticipant(451);
    participant.setCompanyName("test company 1");
    var invitationToTenderParticipants = List.of(participant);
    var form = new ActualTenderActivityForm();

    when(actualTenderActivityService.getById(actualTenderActivity.getId()))
        .thenReturn(actualTenderActivity);
    when(invitationToTenderParticipantService.getInvitationToTenderParticipants(actualTenderActivity))
        .thenReturn(invitationToTenderParticipants);
    when(actualTenderActivityFormService.getForm(actualTenderActivity, invitationToTenderParticipants))
        .thenReturn(form);

    mockMvc.perform(get(
        ReverseRouter.route(on(ActualTenderActivityController.class)
            .renderExistingActualTenderActivityForm(scap.getScapId(), actualTenderActivity.getId()))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/actualtender/actualTenderActivityDetails"))
        .andExpect(model().attribute("backLinkUrl", ReverseRouter.route(on(ActualTenderSummaryController.class)
            .renderActualTenderSummary(scap.getScapId()))))
        .andExpect(model().attribute("form", form))
        .andExpect(model().attribute("remunerationModels", RemunerationModel.getRemunerationModels()))
        .andExpect(model().attribute("contractStages", ContractStage.getContractStages()))
        .andExpect(model().attribute("scopeTitleMaxLength",
            ActualTenderActivityFormValidator.MAX_SCOPE_TITLE_LENGTH.toString()));
  }

  @Test
  void renderExistingActualTenderActivityForm_HasContractingPerformance() throws Exception {
    var actualTenderActivity = new ActualTenderActivity(45);
    var participant = new InvitationToTenderParticipant(451);
    participant.setCompanyName("test company 1");
    var invitationToTenderParticipants = List.of(participant);
    var form = new ActualTenderActivityForm();

    when(actualTenderActivityService.getById(actualTenderActivity.getId()))
        .thenReturn(actualTenderActivity);
    when(invitationToTenderParticipantService.getInvitationToTenderParticipants(actualTenderActivity))
        .thenReturn(invitationToTenderParticipants);
    when(actualTenderActivityFormService.getForm(actualTenderActivity, invitationToTenderParticipants))
        .thenReturn(form);
    when(contractingPerformanceService.hasContractingPerformance(actualTenderActivity)).thenReturn(true);

    mockMvc.perform(get(
        ReverseRouter.route(on(ActualTenderActivityController.class)
            .renderExistingActualTenderActivityForm(scap.getScapId(), actualTenderActivity.getId()))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/actualtender/actualTenderActivityDetails"))
        .andExpect(model().attribute("backLinkUrl", ReverseRouter.route(on(ActualTenderSummaryController.class)
            .renderActualTenderSummary(scap.getScapId()))))
        .andExpect(model().attribute("form", form))
        .andExpect(model().attribute("remunerationModels", RemunerationModel.getRemunerationModels()))
        .andExpect(model().attribute("contractStages", ContractStage.getContractStages()))
        .andExpect(model().attribute("scopeTitleMaxLength",
            ActualTenderActivityFormValidator.MAX_SCOPE_TITLE_LENGTH.toString()))
        .andExpect(model().attribute("contractingPerformanceWarning",
            ActualTenderActivityController.DELETES_CONTRACTING_PERFORMANCE_WARNING));
  }

  @Test
  void saveExistingActualTenderActivityForm_NoErrors_VerifyUpdates() throws Exception {
    var actualTenderActivity = new ActualTenderActivity(45);
    actualTenderActivity.setContractStage(ContractStage.REQUEST_FOR_INFORMATION);
    var form = new ActualTenderActivityForm();
    var bindingResultWithoutErrors = new BeanPropertyBindingResult(form, "form");
    var expectedRedirectUrl = ReverseRouter.route(on(ActualTenderSummaryController.class)
        .renderActualTenderSummary(scap.getScapId()));

    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(actualTenderService.getByScapDetailOrThrow(scapDetail)).thenReturn(actualTender);
    when(actualTenderActivityService.getById(actualTenderActivity.getId())).thenReturn(actualTenderActivity);
    when(actualTenderActivityFormService.validate(eq(form), any(BindingResult.class), eq(actualTender), eq(actualTenderActivity)))
        .thenReturn(bindingResultWithoutErrors);

    mockMvc.perform(post(
        ReverseRouter.route(on(ActualTenderActivityController.class)
            .saveExistingActualTenderActivityForm(scap.getScapId(), actualTenderActivity.getId(), null,
                emptyBindingResult())))
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().is3xxRedirection())
        .andExpect(ControllerTestingUtil.redirectUrl(expectedRedirectUrl));

    verify(updateActualTenderActivityService).updateActualTenderActivity(actualTenderActivity, form);
  }

  @Test
  void saveExistingActualTenderActivityForm_HasErrors_VerifyNeverUpdates() throws Exception {
    var actualTenderActivity = new ActualTenderActivity(45);
    var form = new ActualTenderActivityForm();
    var bindingResultWithErrors = new BeanPropertyBindingResult(form, "form");
    bindingResultWithErrors.addError(
        new FieldError("form", "testField", "test error message")
    );

    when(actualTenderActivityService.getById(actualTenderActivity.getId()))
        .thenReturn(actualTenderActivity);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(actualTenderService.getByScapDetailOrThrow(scapDetail)).thenReturn(actualTender);
    when(actualTenderActivityFormService.validate(eq(form), any(BindingResult.class), eq(actualTender), eq(actualTenderActivity)))
        .thenReturn(bindingResultWithErrors);

    mockMvc.perform(post(
        ReverseRouter.route(on(ActualTenderActivityController.class)
            .saveExistingActualTenderActivityForm(scap.getScapId(), actualTenderActivity.getId(), null,
                emptyBindingResult())))
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/actualtender/actualTenderActivityDetails"))
        .andExpect(model().attribute("backLinkUrl", ReverseRouter.route(on(ActualTenderSummaryController.class)
            .renderActualTenderSummary(scap.getScapId()))))
        .andExpect(model().attribute("form", form))
        .andExpect(model().attribute("remunerationModels", RemunerationModel.getRemunerationModels()))
        .andExpect(model().attribute("contractStages", ContractStage.getContractStages()))
        .andExpect(model().attributeExists("errorList"))
        .andExpect(model().attribute("scopeTitleMaxLength",
            ActualTenderActivityFormValidator.MAX_SCOPE_TITLE_LENGTH.toString()));

    verify(updateActualTenderActivityService, never()).updateActualTenderActivity(any(), any());
  }
}
