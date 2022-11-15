package uk.co.nstauthority.scap.scap.actualtender.activity.bidparticipants;

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

import java.util.List;
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
import uk.co.nstauthority.scap.AbstractControllerTest;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.actualtender.ActualTenderControllerRedirectionServiceTestConfig;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivity;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivityService;
import uk.co.nstauthority.scap.scap.actualtender.activity.ContractStage;
import uk.co.nstauthority.scap.scap.actualtender.activity.InvitationToTenderParticipant;
import uk.co.nstauthority.scap.scap.actualtender.activity.InvitationToTenderParticipantService;
import uk.co.nstauthority.scap.scap.actualtender.activity.awardedcontract.AwardedContractController;
import uk.co.nstauthority.scap.scap.actualtender.summary.ActualTenderSummaryController;
import uk.co.nstauthority.scap.scap.scap.Scap;
import uk.co.nstauthority.scap.scap.scap.ScapService;
import uk.co.nstauthority.scap.scap.tasklist.TaskListController;

@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes = BidParticipantsController.class)
@WithMockUser
@Import(ActualTenderControllerRedirectionServiceTestConfig.class)
class BidParticipantsControllerTest extends AbstractControllerTest {

  @MockBean
  ScapService scapService;

  @MockBean
  ActualTenderActivityService actualTenderActivityService;

  @MockBean
  BidParticipantsFormService bidParticipantsFormService;

  @MockBean
  InvitationToTenderParticipantService invitationToTenderParticipantService;

  private Scap scap;
  private ActualTenderActivity actualTenderActivity;
  private List<InvitationToTenderParticipant> invitationToTenderParticipants;

  @BeforeEach
  void setup() {
    scap = new Scap(140);
    actualTenderActivity =  new ActualTenderActivity(9140);
    var participant1 = new InvitationToTenderParticipant(1271);
    participant1.setCompanyName("company 1");
    var participant2 = new InvitationToTenderParticipant(1272);
    participant2.setCompanyName("company 2");
    invitationToTenderParticipants = List.of(
        participant1, participant2
    );
  }

  @Test
  void renderBidParticipantsForm() throws Exception {
    when(scapService.getScapById(scap.getId())).thenReturn(scap);
    when(actualTenderActivityService.getById(actualTenderActivity.getId())).thenReturn(actualTenderActivity);
    when(invitationToTenderParticipantService.getInvitationToTenderParticipants(actualTenderActivity))
        .thenReturn(invitationToTenderParticipants);

    mockMvc.perform(
        get(ReverseRouter.route(on(BidParticipantsController.class)
            .renderBidParticipantsForm(scap.getId(), actualTenderActivity.getId(), null))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/actualtender/bidParticipants"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(scap.getId()))))
        .andExpect(model().attribute("bidParticipantCheckboxes",
            BidParticipantsFormService.getBidParticipantsCheckboxes(invitationToTenderParticipants)));
  }

  @Test
  void saveBidParticipantsForm_NoErrors_AssertRedirect() throws Exception {
    var form = new BidParticipantsForm();
    form.setSelectedBidParticipantIds(List.of(1272));
    var bindingResult = new BeanPropertyBindingResult(form, "form");
    var expectedRedirectUrl = ReverseRouter.route(on(ActualTenderSummaryController.class)
        .renderActualTenderSummary(scap.getId()));

    when(scapService.getScapById(scap.getId())).thenReturn(scap);
    when(actualTenderActivityService.getById(actualTenderActivity.getId())).thenReturn(actualTenderActivity);
    when(invitationToTenderParticipantService.getInvitationToTenderParticipants(actualTenderActivity))
        .thenReturn(invitationToTenderParticipants);
    when(bidParticipantsFormService.validate(eq(form), any(BindingResult.class), eq(invitationToTenderParticipants)))
        .thenReturn(bindingResult);

    mockMvc.perform(post(
        ReverseRouter.route(on(BidParticipantsController.class)
            .renderBidParticipantsForm(scap.getId(), actualTenderActivity.getId(), null)))
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().is3xxRedirection())
        .andExpect(view().name(String.format("redirect:%s", expectedRedirectUrl)));

    verify(invitationToTenderParticipantService).updateBidParticipants(invitationToTenderParticipants, form.getSelectedBidParticipantIds());
  }

  @Test
  void saveBidParticipantsForm_ContractAwarded_AssertRedirect() throws Exception {
    actualTenderActivity.setContractStage(ContractStage.CONTRACT_AWARDED);
    var form = new BidParticipantsForm();
    form.setSelectedBidParticipantIds(List.of(1272));
    var bindingResult = new BeanPropertyBindingResult(form, "form");
    var expectedRedirectUrl = ReverseRouter.route(on(AwardedContractController.class)
        .renderAwardedContractForm(scap.getId(), actualTenderActivity.getId()));

    when(scapService.getScapById(scap.getId())).thenReturn(scap);
    when(actualTenderActivityService.getById(actualTenderActivity.getId())).thenReturn(actualTenderActivity);
    when(invitationToTenderParticipantService.getInvitationToTenderParticipants(actualTenderActivity))
        .thenReturn(invitationToTenderParticipants);
    when(bidParticipantsFormService.validate(eq(form), any(BindingResult.class), eq(invitationToTenderParticipants)))
        .thenReturn(bindingResult);

    mockMvc.perform(post(
        ReverseRouter.route(on(BidParticipantsController.class)
            .renderBidParticipantsForm(scap.getId(), actualTenderActivity.getId(), null)))
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().is3xxRedirection())
        .andExpect(view().name(String.format("redirect:%s", expectedRedirectUrl)));

    verify(invitationToTenderParticipantService)
        .updateBidParticipants(invitationToTenderParticipants, form.getSelectedBidParticipantIds());
  }

  @Test
  void saveBidParticipants_FormErrors_AssertNoRedirect() throws Exception {
    var form = new BidParticipantsForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");
    bindingResult.addError(new FieldError("form", "testField", "Test validation error"));

    when(scapService.getScapById(scap.getId())).thenReturn(scap);
    when(actualTenderActivityService.getById(actualTenderActivity.getId())).thenReturn(actualTenderActivity);
    when(invitationToTenderParticipantService.getInvitationToTenderParticipants(actualTenderActivity))
        .thenReturn(invitationToTenderParticipants);
    when(bidParticipantsFormService.validate(eq(form), any(BindingResult.class), eq(invitationToTenderParticipants)))
        .thenReturn(bindingResult);

    mockMvc.perform(post(
        ReverseRouter.route(on(BidParticipantsController.class)
            .renderBidParticipantsForm(scap.getId(), actualTenderActivity.getId(), null)))
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/actualtender/bidParticipants"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(scap.getId()))))
        .andExpect(model().attribute("bidParticipantCheckboxes",
            BidParticipantsFormService.getBidParticipantsCheckboxes(invitationToTenderParticipants)))
        .andExpect(model().attributeExists("errorList"));

    verify(invitationToTenderParticipantService, never()).updateBidParticipants(any(), any());
  }
}
