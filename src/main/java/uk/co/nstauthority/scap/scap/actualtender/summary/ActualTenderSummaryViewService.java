package uk.co.nstauthority.scap.scap.actualtender.summary;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;
import javax.transaction.Transactional;
import org.springframework.stereotype.Repository;
import uk.co.fivium.energyportalapi.generated.types.Country;
import uk.co.nstauthority.scap.energyportal.CountryService;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivity;
import uk.co.nstauthority.scap.scap.actualtender.activity.InvitationToTenderParticipant;
import uk.co.nstauthority.scap.scap.actualtender.activity.InvitationToTenderParticipantService;
import uk.co.nstauthority.scap.scap.actualtender.activity.awardedcontract.AwardedContract;
import uk.co.nstauthority.scap.scap.actualtender.activity.awardedcontract.AwardedContractService;

@Repository
public class ActualTenderSummaryViewService {

  public static final String REQUEST_PURPOSE = "Get summary of actual tendering activities for SCAP";

  private final InvitationToTenderParticipantService invitationToTenderParticipantService;
  private final AwardedContractService awardedContractService;
  private final CountryService countryService;

  ActualTenderSummaryViewService(InvitationToTenderParticipantService invitationToTenderParticipantService,
                                 AwardedContractService awardedContractService, CountryService countryService) {
    this.invitationToTenderParticipantService = invitationToTenderParticipantService;
    this.awardedContractService = awardedContractService;
    this.countryService = countryService;
  }

  public ActualTenderSummaryView getSingleViewByActualTenderActivity(ActualTenderActivity actualTenderActivity,
                                                                     Integer scapId) {
    return getByActualTenderActivities(List.of(actualTenderActivity), scapId).get(0);
  }

  @Transactional
  List<ActualTenderSummaryView> getByActualTenderActivities(List<ActualTenderActivity> activities, Integer scapId) {
    var invitationToTenderParticipants = invitationToTenderParticipantService
        .getInvitationToTenderParticipantsForActivities(activities);
    var bidParticipants = InvitationToTenderParticipantService
        .getBidParticipantsFromInvitationToTenderParticipants(invitationToTenderParticipants);

    var invitationToTenderParticipantsMap = getActivityParticipantsMap(invitationToTenderParticipants);
    var bidParticipantsMap = getActivityParticipantsMap(bidParticipants);

    var awardedContracts = awardedContractService.getByActualTenderActivityIn(activities);

    var countries = countryService.getCountriesByIds(getCountryIds(awardedContracts), REQUEST_PURPOSE);
    var countriesMap = getCountriesMap(countries);

    return activities.stream()
        .map(actualTenderActivity -> {
          var activityInvitationToTenderParticipants = invitationToTenderParticipantsMap.get(actualTenderActivity);
          var activityBidParticipants = bidParticipantsMap.getOrDefault(actualTenderActivity, Collections.emptyList());
          var awardedContract = getAwardedContractForActivity(actualTenderActivity, awardedContracts);
          var awardedContractView = awardedContract.map(awardedContract1 ->
                  fromAwardedContract(awardedContract1, countriesMap))
              .orElse(null);
          return new ActualTenderSummaryView(
              scapId,
              actualTenderActivity.getId(),
              actualTenderActivity.getScopeTitle(),
              actualTenderActivity.getScopeDescription(),
              actualTenderActivity.getRemunerationModel(),
              actualTenderActivity.getRemunerationModelName(),
              actualTenderActivity.getContractStage(),
              getParticipantNames(activityInvitationToTenderParticipants),
              getParticipantNames(activityBidParticipants),
              awardedContractView);
        })
        .toList();
  }

  private Map<ActualTenderActivity, List<InvitationToTenderParticipant>> getActivityParticipantsMap(
      List<InvitationToTenderParticipant> participants) {

    return participants.stream()
        .collect(Collectors.groupingBy(InvitationToTenderParticipant::getActualTenderActivity));
  }

  private List<String> getParticipantNames(List<InvitationToTenderParticipant> participants) {
    return participants.stream()
        .map(InvitationToTenderParticipant::getCompanyName)
        .toList();
  }

  private Optional<AwardedContract> getAwardedContractForActivity(ActualTenderActivity activity,
                                                                  List<AwardedContract> awardedContracts) {
    return awardedContracts.stream()
        .filter(awardedContract -> awardedContract.getActualTenderActivity().equals(activity))
        .findFirst();
  }

  private AwardedContractSummaryView fromAwardedContract(AwardedContract awardedContract,
                                                         Map<Integer, String> countriesMap) {
    return new AwardedContractSummaryView(
        awardedContract.getPreferredBidder().getCompanyName(),
        awardedContract.getAwardValue(),
        awardedContract.getAwardRationale(),
        countriesMap.get(awardedContract.getPreferredBidderLocation())
    );
  }

  private List<Integer> getCountryIds(List<AwardedContract> awardedContracts) {
    return awardedContracts.stream()
        .map(AwardedContract::getPreferredBidderLocation)
        .toList();
  }

  private Map<Integer, String> getCountriesMap(List<Country> countries) {
    return countries.stream()
        .collect(Collectors.toMap(
            Country::getCountryId,
            Country::getCountryName
        ));
  }
}
