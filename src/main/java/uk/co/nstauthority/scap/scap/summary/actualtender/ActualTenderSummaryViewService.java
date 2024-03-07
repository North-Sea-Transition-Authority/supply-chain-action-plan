package uk.co.nstauthority.scap.scap.summary.actualtender;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.lang.Nullable;
import org.springframework.stereotype.Service;
import uk.co.fivium.energyportalapi.generated.types.Country;
import uk.co.fivium.formlibrary.validator.date.DateUtils;
import uk.co.nstauthority.scap.energyportal.CountryService;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivity;
import uk.co.nstauthority.scap.scap.actualtender.activity.InvitationToTenderParticipant;
import uk.co.nstauthority.scap.scap.actualtender.activity.InvitationToTenderParticipantService;
import uk.co.nstauthority.scap.scap.actualtender.activity.awardedcontract.AwardedContract;
import uk.co.nstauthority.scap.scap.actualtender.activity.awardedcontract.AwardedContractService;
import uk.co.nstauthority.scap.scap.scap.ScapId;

@Service
public class ActualTenderSummaryViewService {

  public static final String REQUEST_PURPOSE = "Get summary of actual tender activities for SCAP";

  private final InvitationToTenderParticipantService invitationToTenderParticipantService;
  private final AwardedContractService awardedContractService;
  private final CountryService countryService;
  private final ActualTenderSummaryValidationService actualTenderSummaryValidationService;

  @Autowired
  ActualTenderSummaryViewService(InvitationToTenderParticipantService invitationToTenderParticipantService,
                                 AwardedContractService awardedContractService, CountryService countryService,
                                 ActualTenderSummaryValidationService actualTenderSummaryValidationService) {
    this.invitationToTenderParticipantService = invitationToTenderParticipantService;
    this.awardedContractService = awardedContractService;
    this.countryService = countryService;
    this.actualTenderSummaryValidationService = actualTenderSummaryValidationService;
  }

  public ActualTenderActivitySummaryView getSingleViewByActualTenderActivity(ActualTenderActivity actualTenderActivity,
                                                                     ScapId scapId) {
    return getByActualTenderActivities(List.of(actualTenderActivity), scapId).get(0);
  }

  public List<ActualTenderActivitySummaryView> getByActualTenderActivities(List<ActualTenderActivity> activities,
                                                                           ScapId scapId) {
    var invitationToTenderParticipants = invitationToTenderParticipantService
        .getInvitationToTenderParticipantsForActivities(activities);
    var invitationToTenderParticipantsMap = getActivityParticipantsMap(invitationToTenderParticipants);

    var bidParticipants = InvitationToTenderParticipantService
        .getBidParticipantsFromInvitationToTenderParticipants(invitationToTenderParticipants);
    var bidParticipantsMap = getActivityParticipantsMap(bidParticipants);

    var awardedContracts = awardedContractService.getByActualTenderActivityIn(activities);
    var awardedContractsMap = getAwardedContractsMap(awardedContracts);

    var countries = countryService.getCountriesByIds(getCountryIds(awardedContracts), REQUEST_PURPOSE);
    var countriesMap = getCountriesMap(countries);

    return activities.stream()
        .map(actualTenderActivity -> {
          var activityInvitationToTenderParticipants = invitationToTenderParticipantsMap
              .getOrDefault(actualTenderActivity.getId(), Collections.emptyList());
          var activityBidParticipants = bidParticipantsMap.getOrDefault(actualTenderActivity.getId(), Collections.emptyList());
          var awardedContractOpt = getAwardedContract(awardedContractsMap, actualTenderActivity);
          var awardedContractView = awardedContractOpt.map(
              awardedContract -> new AwardedContractSummaryView(
                  getCompanyName(awardedContract.getPreferredBidder()),
                  awardedContract.getAwardValue(),
                  awardedContract.getAwardRationale(),
                  countriesMap.getOrDefault(awardedContract.getPreferredBidderCountryId(), ""),
                  DateUtils.format(awardedContract.getContractAwardDate()),
                  awardedContract.getPaymentTerms(),
                  DateUtils.format(awardedContract.getForecastExecutionStartDate()),
                  DateUtils.format(awardedContract.getForecastExecutionEndDate()))
              ).orElse(null);

          var awardedContract = awardedContractOpt.orElse(null);
          var isValid = actualTenderSummaryValidationService.isValid(
              actualTenderActivity.getActualTender(),
              actualTenderActivity,
              activityInvitationToTenderParticipants,
              activityBidParticipants,
              awardedContract
          );

          return new ActualTenderActivitySummaryView(
              scapId,
              actualTenderActivity.getId(),
              actualTenderActivity.getScopeTitle(),
              actualTenderActivity.getScopeDescription(),
              actualTenderActivity.getRemunerationModel(),
              actualTenderActivity.getRemunerationModelName(),
              actualTenderActivity.getContractStage(),
              getParticipantsAndEpaStatus(activityInvitationToTenderParticipants),
              getParticipantsAndEpaStatus(activityBidParticipants),
              awardedContractView,
              isValid
          );
        })
        .toList();
  }

  private String getCompanyName(@Nullable InvitationToTenderParticipant participant) {
    if (Objects.isNull(participant)) {
      return null;
    }
    return participant.getCompanyName();
  }

  private Map<Integer, List<InvitationToTenderParticipant>> getActivityParticipantsMap(
      List<InvitationToTenderParticipant> participants) {

    return participants.stream()
        .collect(Collectors.groupingBy(participant -> participant.getActualTenderActivity().getId()));
  }

  private Map<String, Boolean> getParticipantsAndEpaStatus(List<InvitationToTenderParticipant> participants) {
    return participants
        .stream()
        .distinct()
        .collect(Collectors.toMap(
            InvitationToTenderParticipant::getCompanyName,
            participant -> Objects.isNull(participant.getOrganisationUnitId()),
            (participant1, participant2) -> participant1 && participant2)
        );
  }

  private List<Integer> getCountryIds(List<AwardedContract> awardedContracts) {
    return awardedContracts.stream()
        .map(AwardedContract::getPreferredBidderCountryId)
        .toList();
  }

  private Map<Integer, String> getCountriesMap(List<Country> countries) {
    return countries.stream()
        .collect(Collectors.toMap(
            Country::getCountryId,
            Country::getCountryName
        ));
  }

  private Map<Integer, AwardedContract> getAwardedContractsMap(List<AwardedContract> awardedContracts) {
    return awardedContracts.stream()
        .collect(Collectors.toMap(
            awardedContract -> awardedContract.getActualTenderActivity().getId(),
            Function.identity()
        ));
  }

  private Optional<AwardedContract> getAwardedContract(Map<Integer, AwardedContract> awardedContractMap,
                                                       ActualTenderActivity actualTenderActivity) {
    return Optional.ofNullable(awardedContractMap.get(actualTenderActivity.getId()));
  }
}
