package uk.co.nstauthority.scap.scap.actualtender.activity.awardedcontract;

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
import uk.co.nstauthority.scap.AbstractControllerTest;
import uk.co.nstauthority.scap.AbstractScapSubmitterControllerTest;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivity;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivityService;
import uk.co.nstauthority.scap.scap.actualtender.activity.InvitationToTenderParticipant;
import uk.co.nstauthority.scap.scap.actualtender.activity.InvitationToTenderParticipantService;
import uk.co.nstauthority.scap.scap.actualtender.activity.bidparticipants.BidParticipantsController;
import uk.co.nstauthority.scap.scap.actualtender.summary.ActualTenderSummaryController;
import uk.co.nstauthority.scap.scap.scap.Scap;
import uk.co.nstauthority.scap.scap.scap.ScapService;

@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes = AwardedContractController.class)
@WithMockUser
class AwardedContractControllerTest extends AbstractScapSubmitterControllerTest {

  @MockBean
  ActualTenderActivityService actualTenderActivityService;

  @MockBean
  InvitationToTenderParticipantService invitationToTenderParticipantService;

  @MockBean
  AwardedContractFormService awardedContractFormService;

  @MockBean
  AwardedContractService awardedContractService;

  private ActualTenderActivity actualTenderActivity;
  private List<InvitationToTenderParticipant> bidParticipants;
  private Map<String, String> bidParticipantsMap;

  @BeforeEach
  void setup() {
    actualTenderActivity = new ActualTenderActivity(1410);
    var participant1 = new InvitationToTenderParticipant(1411);
    participant1.setCompanyName("Company 1");
    var participant2 = new InvitationToTenderParticipant(1412);
    participant2.setCompanyName("Company 2");
    bidParticipants = List.of(participant1, participant2);
    bidParticipantsMap = Map.of(
        participant1.getId().toString(), participant1.getCompanyName(),
        participant2.getId().toString(), participant2.getCompanyName()
    );
  }

  @Test
  void renderAwardedContractForm_NewAwardedContract() throws Exception {
    when(scapService.getScapById(scap.getId())).thenReturn(scap);
    when(actualTenderActivityService.getById(actualTenderActivity.getId())).thenReturn(actualTenderActivity);
    when(invitationToTenderParticipantService.getBidParticipants(actualTenderActivity)).thenReturn(bidParticipants);
    when(awardedContractService.getByActualTenderActivity(actualTenderActivity)).thenReturn(Optional.empty());

    mockMvc.perform(get(
        ReverseRouter.route(on(AwardedContractController.class)
            .renderAwardedContractForm(scap.getScapId(), actualTenderActivity.getId()))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/actualtender/actualContractAward"))
        .andExpect(model().attributeExists("form"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(BidParticipantsController.class)
                .renderBidParticipantsForm(scap.getScapId(), actualTenderActivity.getId(), null))))
        .andExpect(model().attribute("bidParticipantsMap", bidParticipantsMap))
        .andExpect(model().attribute("countrySearchRestUrl",
            ReverseRouter.route(on(AwardedContractRestController.class).getCountrySearchResults(null))));
  }

  @Test
  void renderAwardedContractForm_ExistingAwardedContract() throws Exception {
    var existingAwardedContract = new AwardedContract(3141);
    var countryId = 4141;
    existingAwardedContract.setPreferredBidderCountryId(countryId);
    var form = new AwardedContractForm();
    var preselectedCountry = Map.of(String.valueOf(countryId), "Test Country");

    when(scapService.getScapById(scap.getId())).thenReturn(scap);
    when(actualTenderActivityService.getById(actualTenderActivity.getId())).thenReturn(actualTenderActivity);
    when(invitationToTenderParticipantService.getBidParticipants(actualTenderActivity)).thenReturn(bidParticipants);
    when(awardedContractService.getByActualTenderActivity(actualTenderActivity))
        .thenReturn(Optional.of(existingAwardedContract));
    when(awardedContractFormService.getForm(existingAwardedContract)).thenReturn(form);
    when(awardedContractFormService.getPreselectedBidderLocation(countryId))
        .thenReturn(Optional.of(preselectedCountry));

    mockMvc.perform(get(
            ReverseRouter.route(on(AwardedContractController.class)
                .renderAwardedContractForm(scap.getScapId(), actualTenderActivity.getId()))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/actualtender/actualContractAward"))
        .andExpect(model().attribute("form", form))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(BidParticipantsController.class)
                .renderBidParticipantsForm(scap.getScapId(), actualTenderActivity.getId(), null))))
        .andExpect(model().attribute("bidParticipantsMap", bidParticipantsMap))
        .andExpect(model().attribute("countrySearchRestUrl",
            ReverseRouter.route(on(AwardedContractRestController.class).getCountrySearchResults(null))))
        .andExpect(model().attribute("preselectedCountry", preselectedCountry));
  }

  @Test
  void saveAwardedContractForm_HasErrors_VerifyNeverSaves() throws Exception {
    var form = new AwardedContractForm();
    form.setPreferredBidderCountryId(0);
    var bindingResultWithErrors = new BeanPropertyBindingResult(form, "form");
    bindingResultWithErrors.addError(new FieldError(
        "form", "testFieldName", "Test error message"
    ));
    var preselectedCountry = Map.of(String.valueOf(form.getPreferredBidderCountryId()), "United Kingdom");

    when(scapService.getScapById(scap.getId())).thenReturn(scap);
    when(actualTenderActivityService.getById(actualTenderActivity.getId())).thenReturn(actualTenderActivity);
    when(invitationToTenderParticipantService.getBidParticipants(actualTenderActivity)).thenReturn(bidParticipants);
    when(awardedContractFormService.validate(eq(form), any(BindingResult.class), eq(bidParticipants)))
        .thenReturn(bindingResultWithErrors);
    when(awardedContractFormService.getPreselectedBidderLocationFromForm(form.getPreferredBidderCountryId(), bindingResultWithErrors))
        .thenReturn(Optional.of(preselectedCountry));

    mockMvc.perform(post(
        ReverseRouter.route(on(AwardedContractController.class)
            .saveAwardedContractForm(scap.getScapId(), actualTenderActivity.getId(), null, emptyBindingResult())))
            .flashAttr("form", form)
            .with(csrf()))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/actualtender/actualContractAward"))
        .andExpect(model().attribute("form", form))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(BidParticipantsController.class)
                .renderBidParticipantsForm(scap.getScapId(), actualTenderActivity.getId(), null))))
        .andExpect(model().attribute("bidParticipantsMap", bidParticipantsMap))
        .andExpect(model().attribute("countrySearchRestUrl",
            ReverseRouter.route(on(AwardedContractRestController.class).getCountrySearchResults(null))))
        .andExpect(model().attribute("preselectedCountry", preselectedCountry));

    verify(awardedContractService, never()).saveAwardedContract(any(), any(), any());
  }

  @Test
  void saveAwardedContractForm_HasNoErrors_VerifySaves() throws Exception {
    var form = new AwardedContractForm();
    var bindingResultWithoutErrors = new BeanPropertyBindingResult(form, "form");
    var expectedRedirectUrl = ReverseRouter.route(on(ActualTenderSummaryController.class)
        .renderActualTenderSummary(scap.getScapId()));

    when(scapService.getScapById(scap.getId())).thenReturn(scap);
    when(actualTenderActivityService.getById(actualTenderActivity.getId())).thenReturn(actualTenderActivity);
    when(invitationToTenderParticipantService.getBidParticipants(actualTenderActivity)).thenReturn(bidParticipants);
    when(awardedContractFormService.validate(eq(form), any(BindingResult.class), eq(bidParticipants)))
        .thenReturn(bindingResultWithoutErrors);

    mockMvc.perform(post(
        ReverseRouter.route(on(AwardedContractController.class)
            .saveAwardedContractForm(scap.getScapId(), actualTenderActivity.getId(), null, emptyBindingResult())))
        .flashAttr("form", form)
        .with(csrf()))
        .andExpect(status().is3xxRedirection())
        .andExpect(view().name(String.format("redirect:%s", expectedRedirectUrl)));

    verify(awardedContractService).saveAwardedContract(actualTenderActivity, form, bidParticipants);
  }
}
