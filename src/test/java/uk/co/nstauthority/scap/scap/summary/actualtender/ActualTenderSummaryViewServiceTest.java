package uk.co.nstauthority.scap.scap.summary.actualtender;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.tuple;
import static org.mockito.Mockito.when;

import java.math.BigDecimal;
import java.time.Instant;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.fivium.energyportalapi.generated.types.Country;
import uk.co.nstauthority.scap.energyportal.CountryService;
import uk.co.nstauthority.scap.scap.RemunerationModel;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivity;
import uk.co.nstauthority.scap.scap.actualtender.activity.ContractStage;
import uk.co.nstauthority.scap.scap.actualtender.activity.InvitationToTenderParticipant;
import uk.co.nstauthority.scap.scap.actualtender.activity.InvitationToTenderParticipantService;
import uk.co.nstauthority.scap.scap.actualtender.activity.awardedcontract.AwardedContract;
import uk.co.nstauthority.scap.scap.actualtender.activity.awardedcontract.AwardedContractBuilder;
import uk.co.nstauthority.scap.scap.actualtender.activity.awardedcontract.AwardedContractService;
import uk.co.nstauthority.scap.scap.scap.ScapId;

@ExtendWith(MockitoExtension.class)
class ActualTenderSummaryViewServiceTest {

  @Mock
  InvitationToTenderParticipantService invitationToTenderParticipantService;

  @Mock
  AwardedContractService awardedContractService;

  @Mock
  CountryService countryService;

  @Mock
  ActualTenderSummaryValidationService actualTenderSummaryValidationService;

  @InjectMocks
  ActualTenderSummaryViewService actualTenderSummaryViewService;


  @Test
  void getSingleViewByActualTenderActivity() {
    var scapId = new ScapId(173);

    var actualTenderActivity = new ActualTenderActivity(10);
    actualTenderActivity.setScopeTitle("test scope title 1");
    actualTenderActivity.setScopeDescription("test scope description 1");
    actualTenderActivity.setRemunerationModel(RemunerationModel.LUMP_SUM);
    actualTenderActivity.setContractStage(ContractStage.CONTRACT_AWARDED);
    var participant = new InvitationToTenderParticipant(210);
    participant.setCompanyName("company name 1");
    participant.setActualTenderActivity(actualTenderActivity);
    participant.setBidParticipant(true);
    var participants = List.of(participant);
    var awardedContract = AwardedContractBuilder.newBuilder()
        .withPreferredBidder(null)
        .withActualTenderActivity(actualTenderActivity)
        .build();

    when(invitationToTenderParticipantService
        .getInvitationToTenderParticipantsForActivities(List.of(actualTenderActivity)))
        .thenReturn(participants);
    when(awardedContractService.getByActualTenderActivityIn(Collections.singletonList(actualTenderActivity)))
        .thenReturn(Collections.singletonList(awardedContract));

    var view = actualTenderSummaryViewService.getSingleViewByActualTenderActivity(actualTenderActivity, scapId);

    assertThat(view).extracting(
        ActualTenderActivitySummaryView::scapId,
        ActualTenderActivitySummaryView::activityId,
        ActualTenderActivitySummaryView::scopeTitle,
        ActualTenderActivitySummaryView::scopeDescription,
        ActualTenderActivitySummaryView::remunerationModel,
        ActualTenderActivitySummaryView::remunerationModelName,
        ActualTenderActivitySummaryView::contractStage,
        ActualTenderActivitySummaryView::ittParticipants,
        ActualTenderActivitySummaryView::bidParticipants,
        summaryView -> summaryView.awardedContractSummaryView().preferredBidderName(),
        summaryView -> summaryView.awardedContractSummaryView().awardValue(),
        summaryView -> summaryView.awardedContractSummaryView().awardRationale(),
        summaryView -> summaryView.awardedContractSummaryView().preferredBidderCountry()
    ).containsExactly(
        scapId,
        actualTenderActivity.getId(),
        actualTenderActivity.getScopeTitle(),
        actualTenderActivity.getScopeDescription(),
        actualTenderActivity.getRemunerationModel(),
        actualTenderActivity.getRemunerationModelName(),
        actualTenderActivity.getContractStage(),
        Map.of(participant.getCompanyName(), true),
        Map.of(participant.getCompanyName(), true),
        null,
        awardedContract.getAwardValue(),
        awardedContract.getAwardRationale(),
        ""
    );
  }

  @Test
  void getDuplicateParticipantByActualTenderActivity() {
    var scapId = new ScapId(173);

    var actualTenderActivity = new ActualTenderActivity(10);
    actualTenderActivity.setScopeTitle("test scope title 1");
    actualTenderActivity.setScopeDescription("test scope description 1");
    actualTenderActivity.setRemunerationModel(RemunerationModel.LUMP_SUM);
    actualTenderActivity.setContractStage(ContractStage.CONTRACT_AWARDED);
    var participant = new InvitationToTenderParticipant(210);
    participant.setCompanyName("company name 1");
    participant.setActualTenderActivity(actualTenderActivity);
    participant.setBidParticipant(true);
    var participant2 = new InvitationToTenderParticipant(211);
    participant2.setCompanyName("company name 1");
    participant2.setActualTenderActivity(actualTenderActivity);
    participant2.setBidParticipant(true);
    var participants = List.of(participant, participant2);
    var awardedContract = AwardedContractBuilder.newBuilder()
        .withPreferredBidder(null)
        .withActualTenderActivity(actualTenderActivity)
        .build();

    when(invitationToTenderParticipantService
        .getInvitationToTenderParticipantsForActivities(List.of(actualTenderActivity)))
        .thenReturn(participants);
    when(awardedContractService.getByActualTenderActivityIn(Collections.singletonList(actualTenderActivity)))
        .thenReturn(Collections.singletonList(awardedContract));

    var view = actualTenderSummaryViewService.getSingleViewByActualTenderActivity(actualTenderActivity, scapId);

    assertThat(view).extracting(

        ActualTenderActivitySummaryView::ittParticipants,
        ActualTenderActivitySummaryView::bidParticipants
    ).containsExactly(
        Map.of(participant.getCompanyName(), true),
        Map.of(participant.getCompanyName(), true)
    );
  }

  @Test
  void getByActualTenderActivities() {
    var scapId = new ScapId(11);

    var actualTenderActivity1 = new ActualTenderActivity(10);
    actualTenderActivity1.setScopeTitle("test scope title 1");
    actualTenderActivity1.setScopeDescription("test scope description 1");
    actualTenderActivity1.setRemunerationModel(RemunerationModel.LUMP_SUM);
    actualTenderActivity1.setContractStage(ContractStage.INVITATION_TO_TENDER_IS_LIVE);
    var participant1 = new InvitationToTenderParticipant(210);
    participant1.setCompanyName("company name 1");
    participant1.setActualTenderActivity(actualTenderActivity1);

    var actualTenderActivity2 = new ActualTenderActivity(11);
    actualTenderActivity2.setScopeTitle("test scope title 2");
    actualTenderActivity2.setScopeDescription("test scope description 2");
    actualTenderActivity2.setRemunerationModel(RemunerationModel.OTHER);
    actualTenderActivity2.setRemunerationModelName("remuneration model name");
    actualTenderActivity2.setContractStage(ContractStage.BID_APPRAISAL);
    var participant2 = new InvitationToTenderParticipant(211);
    participant2.setCompanyName("company name 2");
    participant2.setBidParticipant(true);
    participant2.setActualTenderActivity(actualTenderActivity2);
    var participant3 = new InvitationToTenderParticipant(212);
    participant3.setCompanyName("company name 3");
    participant3.setActualTenderActivity(actualTenderActivity2);

    var actualTenderActivity3 = new ActualTenderActivity(12);
    actualTenderActivity3.setScopeTitle("test scope title 3");
    actualTenderActivity3.setScopeDescription("test scope description 3");
    actualTenderActivity3.setRemunerationModel(RemunerationModel.REIMBURSABLE);
    actualTenderActivity3.setContractStage(ContractStage.CONTRACT_AWARDED);
    var participant4 = new InvitationToTenderParticipant(213);
    participant4.setCompanyName("company name 4");
    participant4.setBidParticipant(true);
    participant4.setActualTenderActivity(actualTenderActivity3);
    var participant5 = new InvitationToTenderParticipant(214);
    participant5.setCompanyName("company name 5");
    participant5.setActualTenderActivity(actualTenderActivity3);
    participant5.setOrganisationUnitId(55);
    var country = new Country(0, "United Kingdom", null, null);
    var awardedContract = new AwardedContract(actualTenderActivity3, Instant.now());
    awardedContract.setPreferredBidder(participant4);
    awardedContract.setAwardValue(BigDecimal.valueOf(1.2));
    awardedContract.setAwardRationale("award rationale");
    awardedContract.setPreferredBidderCountryId(country.getCountryId());

    var actualTenderActivities = List.of(
        actualTenderActivity1, actualTenderActivity2, actualTenderActivity3
    );
    var participants = List.of(
        participant1, participant2, participant3, participant4, participant5
    );

    when(invitationToTenderParticipantService.getInvitationToTenderParticipantsForActivities(actualTenderActivities))
        .thenReturn(participants);
    when(awardedContractService.getByActualTenderActivityIn(actualTenderActivities))
        .thenReturn(List.of(awardedContract));
    when(countryService.getCountriesByIds(List.of(country.getCountryId()), ActualTenderSummaryViewService.REQUEST_PURPOSE))
        .thenReturn(List.of(country));

    var returnedViews = actualTenderSummaryViewService
        .getByActualTenderActivities(actualTenderActivities, scapId);

    assertThat(returnedViews).extracting(
        ActualTenderActivitySummaryView::scapId,
        ActualTenderActivitySummaryView::activityId,
        ActualTenderActivitySummaryView::scopeTitle,
        ActualTenderActivitySummaryView::scopeDescription,
        ActualTenderActivitySummaryView::remunerationModel,
        ActualTenderActivitySummaryView::remunerationModelName,
        ActualTenderActivitySummaryView::contractStage,
        ActualTenderActivitySummaryView::ittParticipants,
        ActualTenderActivitySummaryView::bidParticipants
    ).containsExactly(
        tuple(
            scapId,
            actualTenderActivity1.getId(),
            actualTenderActivity1.getScopeTitle(),
            actualTenderActivity1.getScopeDescription(),
            actualTenderActivity1.getRemunerationModel(),
            actualTenderActivity1.getRemunerationModelName(),
            actualTenderActivity1.getContractStage(),
            Map.of(participant1.getCompanyName(), true),
            Collections.emptyMap()
        ),
        tuple(
            scapId,
            actualTenderActivity2.getId(),
            actualTenderActivity2.getScopeTitle(),
            actualTenderActivity2.getScopeDescription(),
            actualTenderActivity2.getRemunerationModel(),
            actualTenderActivity2.getRemunerationModelName(),
            actualTenderActivity2.getContractStage(),
            Map.of(participant2.getCompanyName(), true, participant3.getCompanyName(), true),
            Map.of(participant2.getCompanyName(), true)
        ),
        tuple(
            scapId,
            actualTenderActivity3.getId(),
            actualTenderActivity3.getScopeTitle(),
            actualTenderActivity3.getScopeDescription(),
            actualTenderActivity3.getRemunerationModel(),
            actualTenderActivity3.getRemunerationModelName(),
            actualTenderActivity3.getContractStage(),
            Map.of(participant4.getCompanyName(), true, participant5.getCompanyName(), false),
            Map.of(participant4.getCompanyName(), true)
        )
    );

    assertThat(returnedViews.get(0).awardedContractSummaryView()).isNull();
    assertThat(returnedViews.get(1).awardedContractSummaryView()).isNull();
    assertThat(returnedViews.get(2)).extracting(
        view -> view.awardedContractSummaryView().preferredBidderName(),
        view -> view.awardedContractSummaryView().awardValue(),
        view -> view.awardedContractSummaryView().awardRationale(),
        view -> view.awardedContractSummaryView().preferredBidderCountry()
    ).containsExactly(
          awardedContract.getPreferredBidder().getCompanyName(),
          awardedContract.getAwardValue(),
          awardedContract.getAwardRationale(),
          country.getCountryName()
    );
  }

}
