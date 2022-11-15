package uk.co.nstauthority.scap.scap.actualtender.summary;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.List;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.fivium.energyportalapi.generated.types.Country;
import uk.co.nstauthority.scap.energyportal.CountryService;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivity;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivityController;
import uk.co.nstauthority.scap.scap.actualtender.activity.ContractStage;
import uk.co.nstauthority.scap.scap.actualtender.activity.InvitationToTenderParticipant;
import uk.co.nstauthority.scap.scap.actualtender.activity.InvitationToTenderParticipantService;
import uk.co.nstauthority.scap.scap.actualtender.activity.awardedcontract.AwardedContract;
import uk.co.nstauthority.scap.scap.actualtender.activity.awardedcontract.AwardedContractService;
import uk.co.nstauthority.scap.scap.actualtender.activity.delete.DeleteActualTenderActivityController;

@Service
public class ActualTenderSummaryService {

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

  public List<ActualTenderSummaryView> getViewsForActualTenderActivities(List<ActualTenderActivity> actualTenderDetails,
                                                                         Integer scapId) {
    return actualTenderDetails.stream()
        .map(actualTenderActivity -> getActualTenderSummaryView(actualTenderActivity, scapId))
        .toList();
  }

  public ActualTenderSummaryView getActualTenderSummaryView(ActualTenderActivity actualTenderActivity, Integer scapId) {
    var invitationToTenderParticipants = invitationToTenderParticipantService
        .getInvitationToTenderParticipants(actualTenderActivity);
    var bidParticipants = InvitationToTenderParticipantService
        .getBidParticipantsFromInvitationToTenderParticipants(invitationToTenderParticipants);
    AwardedContractSummaryView awardedContractSummaryView;
    if (ContractStage.CONTRACT_AWARDED.equals(actualTenderActivity.getContractStage())) {
      var awardedContract = awardedContractService.getByActualTenderActivity(actualTenderActivity);
      awardedContractSummaryView = awardedContract.map(this::getAwardedContractSummaryView)
          .orElse(null);
    } else {
      awardedContractSummaryView = null;
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
        ReverseRouter.route(on(ActualTenderActivityController.class)
            .renderExistingActualTenderActivityForm(scapId, actualTenderActivity.getId())),
        ReverseRouter.route(on(DeleteActualTenderActivityController.class)
            .renderDeleteActualTenderActivityConfirmation(scapId, actualTenderActivity.getId()))
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
