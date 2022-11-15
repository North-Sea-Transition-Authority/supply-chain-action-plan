package uk.co.nstauthority.scap.scap.actualtender.summary;

import java.util.List;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.fivium.energyportalapi.generated.types.Country;
import uk.co.nstauthority.scap.energyportal.CountryService;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivity;
import uk.co.nstauthority.scap.scap.actualtender.activity.ContractStage;
import uk.co.nstauthority.scap.scap.actualtender.activity.InvitationToTenderParticipant;
import uk.co.nstauthority.scap.scap.actualtender.activity.InvitationToTenderParticipantService;
import uk.co.nstauthority.scap.scap.actualtender.activity.awardedcontract.AwardedContract;
import uk.co.nstauthority.scap.scap.actualtender.activity.awardedcontract.AwardedContractService;

@Service
class ActualTenderSummaryService {

  public static final String AWARDED_CONTRACT_VIEW_REQUEST_PURPOSE =
      "Get country of preferred bidder for actual tendering activity for SCAP";
  private final InvitationToTenderParticipantService invitationToTenderParticipantService;
  private final AwardedContractService awardedContractService;
  private final CountryService countryService;

  @Autowired
  ActualTenderSummaryService(InvitationToTenderParticipantService invitationToTenderParticipantService,
                             AwardedContractService awardedContractService, CountryService countryService) {
    this.invitationToTenderParticipantService = invitationToTenderParticipantService;
    this.awardedContractService = awardedContractService;
    this.countryService = countryService;
  }

  public List<ActualTenderSummaryView> getViewsForActualTenderActivities(List<ActualTenderActivity> actualTenderDetails) {
    return actualTenderDetails.stream()
        .map(this::getActualTenderSummaryView)
        .toList();
  }

  private ActualTenderSummaryView getActualTenderSummaryView(ActualTenderActivity actualTenderActivity) {
    var invitationToTenderParticipants = invitationToTenderParticipantService
        .getInvitationToTenderParticipants(actualTenderActivity);
    var bidParticipants = InvitationToTenderParticipantService
        .getBidParticipantsFromInvitationToTenderParticipants(invitationToTenderParticipants);
    AwardedContractSummaryView awardedContractSummaryView = null;
    if (ContractStage.CONTRACT_AWARDED.equals(actualTenderActivity.getContractStage())) {
      var awardedContract = awardedContractService.getByActualTenderActivity(actualTenderActivity);
      awardedContractSummaryView = awardedContract.map(this::getAwardedContractSummaryView)
          .orElse(null);
    }

    var invitationToTenderParticipantNames = getParticipantNames(invitationToTenderParticipants);
    var bidParticipantNames = getParticipantNames(bidParticipants);
    return new ActualTenderSummaryView(
        actualTenderActivity.getScopeTitle(),
        actualTenderActivity.getScopeDescription(),
        actualTenderActivity.getRemunerationModel(),
        actualTenderActivity.getRemunerationModelName(),
        actualTenderActivity.getContractStage(),
        invitationToTenderParticipantNames,
        bidParticipantNames,
        awardedContractSummaryView,
        "#", "#"
    );
  }

  private AwardedContractSummaryView getAwardedContractSummaryView(AwardedContract awardedContract) {
    var preferredBidderLocation = countryService.findCountryById(
        awardedContract.getPreferredBidderLocation(),
        AWARDED_CONTRACT_VIEW_REQUEST_PURPOSE)
        .map(Country::getCountryName)
        .orElse(null);
    return new AwardedContractSummaryView(
        awardedContract.getPreferredBidder().getCompanyName(),
        awardedContract.getAwardValue(),
        awardedContract.getAwardRationale(),
        preferredBidderLocation
    );
  }

  private List<String> getParticipantNames(List<InvitationToTenderParticipant> participants) {
    return participants.stream()
        .map(InvitationToTenderParticipant::getCompanyName)
        .toList();
  }
}
