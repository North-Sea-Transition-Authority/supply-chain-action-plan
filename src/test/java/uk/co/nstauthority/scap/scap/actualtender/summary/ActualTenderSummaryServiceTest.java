package uk.co.nstauthority.scap.scap.actualtender.summary;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.tuple;
import static org.mockito.Mockito.when;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.math.BigDecimal;
import java.time.Instant;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.fivium.energyportalapi.generated.types.Country;
import uk.co.nstauthority.scap.energyportal.CountryService;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.RemunerationModel;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivity;
import uk.co.nstauthority.scap.scap.actualtender.activity.ContractStage;
import uk.co.nstauthority.scap.scap.actualtender.activity.InvitationToTenderParticipant;
import uk.co.nstauthority.scap.scap.actualtender.activity.InvitationToTenderParticipantService;
import uk.co.nstauthority.scap.scap.actualtender.activity.awardedcontract.AwardedContract;
import uk.co.nstauthority.scap.scap.actualtender.activity.awardedcontract.AwardedContractService;
import uk.co.nstauthority.scap.scap.actualtender.activity.delete.DeleteActualTenderActivityController;

@ExtendWith(MockitoExtension.class)
class ActualTenderSummaryServiceTest {

  @Mock
  InvitationToTenderParticipantService invitationToTenderParticipantService;

  @Mock
  AwardedContractService awardedContractService;

  @Mock
  CountryService countryService;

  @InjectMocks
  ActualTenderSummaryService actualTenderSummaryService;

  @Test
  void getViewsForActualTenderActivities() {
    var scapId = 11;

    var actualTenderActivity1 = new ActualTenderActivity(10);
    actualTenderActivity1.setScopeTitle("test scope title 1");
    actualTenderActivity1.setScopeDescription("test scope description 1");
    actualTenderActivity1.setRemunerationModel(RemunerationModel.LUMP_SUM);
    actualTenderActivity1.setContractStage(ContractStage.REQUEST_FOR_INFORMATION);
    var participant1 = new InvitationToTenderParticipant(210);
    participant1.setCompanyName("company name 1");
    var participants1 = List.of(participant1);

    var actualTenderActivity2 = new ActualTenderActivity(11);
    actualTenderActivity2.setScopeTitle("test scope title 2");
    actualTenderActivity2.setScopeDescription("test scope description 2");
    actualTenderActivity2.setRemunerationModel(RemunerationModel.OTHER);
    actualTenderActivity2.setRemunerationModelName("remuneration model name");
    actualTenderActivity2.setContractStage(ContractStage.INVITATION_TO_TENDER);
    var participant2 = new InvitationToTenderParticipant(211);
    participant2.setCompanyName("company name 2");
    participant2.setBidParticipant(true);
    var participant3 = new InvitationToTenderParticipant(212);
    participant3.setCompanyName("company name 3");
    var participants2 = List.of(participant2, participant3);

    var actualTenderActivity3 = new ActualTenderActivity(12);
    actualTenderActivity3.setScopeTitle("test scope title 3");
    actualTenderActivity3.setScopeDescription("test scope description 3");
    actualTenderActivity3.setRemunerationModel(RemunerationModel.REIMBURSABLE);
    actualTenderActivity3.setContractStage(ContractStage.CONTRACT_AWARDED);
    var participant4 = new InvitationToTenderParticipant(213);
    participant4.setCompanyName("company name 4");
    participant4.setBidParticipant(true);
    var participant5 = new InvitationToTenderParticipant(214);
    participant5.setCompanyName("company name 5");
    var participants3 = List.of(participant4, participant5);
    var country = new Country(0, "United Kingdom", null, null);
    var awardedContract = new AwardedContract(actualTenderActivity3, Instant.now());
    awardedContract.setPreferredBidder(participant4);
    awardedContract.setAwardValue(BigDecimal.valueOf(1.2));
    awardedContract.setAwardRationale("award rationale");
    awardedContract.setPreferredBidderLocation(country.getCountryId());

    var actualTenderActivities = List.of(
        actualTenderActivity1, actualTenderActivity2, actualTenderActivity3
    );

    when(invitationToTenderParticipantService.getInvitationToTenderParticipants(actualTenderActivity1))
        .thenReturn(participants1);
    when(invitationToTenderParticipantService.getInvitationToTenderParticipants(actualTenderActivity2))
        .thenReturn(participants2);
    when(invitationToTenderParticipantService.getInvitationToTenderParticipants(actualTenderActivity3))
        .thenReturn(participants3);
    when(awardedContractService.getByActualTenderActivity(actualTenderActivity3))
        .thenReturn(Optional.of(awardedContract));
    when(countryService
        .findCountryById(country.getCountryId(), ActualTenderSummaryService.AWARDED_CONTRACT_VIEW_REQUEST_PURPOSE))
        .thenReturn(Optional.of(country));

    var returnedViews = actualTenderSummaryService
        .getViewsForActualTenderActivities(actualTenderActivities, scapId);

    assertThat(returnedViews).extracting(
        ActualTenderSummaryView::scopeTitle,
        ActualTenderSummaryView::scopeDescription,
        ActualTenderSummaryView::remunerationModel,
        ActualTenderSummaryView::remunerationModelName,
        ActualTenderSummaryView::contractStage,
        ActualTenderSummaryView::invitationToTenderParticipants,
        ActualTenderSummaryView::bidParticipants,
        ActualTenderSummaryView::changeLinkUrl,
        ActualTenderSummaryView::deleteLinkUrl
    ).containsExactly(
        tuple(
            actualTenderActivity1.getScopeTitle(),
            actualTenderActivity1.getScopeDescription(),
            actualTenderActivity1.getRemunerationModel(),
            actualTenderActivity1.getRemunerationModelName(),
            actualTenderActivity1.getContractStage(),
            List.of(participant1.getCompanyName()),
            Collections.emptyList(),
            "#",
            ReverseRouter.route(on(DeleteActualTenderActivityController.class)
                .renderDeleteActualTenderActivityConfirmation(scapId, actualTenderActivity1.getId()))
        ),
        tuple(
            actualTenderActivity2.getScopeTitle(),
            actualTenderActivity2.getScopeDescription(),
            actualTenderActivity2.getRemunerationModel(),
            actualTenderActivity2.getRemunerationModelName(),
            actualTenderActivity2.getContractStage(),
            List.of(participant2.getCompanyName(), participant3.getCompanyName()),
            List.of(participant2.getCompanyName()),
            "#",
            ReverseRouter.route(on(DeleteActualTenderActivityController.class)
                .renderDeleteActualTenderActivityConfirmation(scapId, actualTenderActivity2.getId()))
        ),
        tuple(
            actualTenderActivity3.getScopeTitle(),
            actualTenderActivity3.getScopeDescription(),
            actualTenderActivity3.getRemunerationModel(),
            actualTenderActivity3.getRemunerationModelName(),
            actualTenderActivity3.getContractStage(),
            List.of(participant4.getCompanyName(), participant5.getCompanyName()),
            List.of(participant4.getCompanyName()),
            "#",
            ReverseRouter.route(on(DeleteActualTenderActivityController.class)
                .renderDeleteActualTenderActivityConfirmation(scapId, actualTenderActivity3.getId()))
        )
    );

    assertThat(returnedViews.get(0).awardedContract()).isNull();
    assertThat(returnedViews.get(1).awardedContract()).isNull();
    assertThat(returnedViews.get(2)).extracting(
        view -> view.awardedContract().preferredBidder(),
        view -> view.awardedContract().awardValue(),
        view -> view.awardedContract().awardRationale(),
        view -> view.awardedContract().preferredBidderLocation()
    ).containsExactly(
          awardedContract.getPreferredBidder().getCompanyName(),
          awardedContract.getAwardValue(),
          awardedContract.getAwardRationale(),
          country.getCountryName()
    );
  }

}
