package uk.co.nstauthority.scap.scap.actualtender.activity;

import java.time.Clock;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.energyportal.OrganisationUnitService;
import uk.co.nstauthority.scap.fds.searchselector.ManualEntryUtil;
import uk.co.nstauthority.scap.scap.actualtender.activity.awardedcontract.AwardedContractService;

@Service
public class InvitationToTenderParticipantService {

  private final InvitationToTenderParticipantRepository invitationToTenderParticipantRepository;
  private final OrganisationUnitService organisationUnitService;
  private final Clock clock;
  private final AwardedContractService awardedContractService;

  static final String ORGANISATION_UNIT_REQUEST_PURPOSE = "Get selected org units for SCAP actual tender activity";

  @Autowired
  InvitationToTenderParticipantService(InvitationToTenderParticipantRepository invitationToTenderParticipantRepository,
                                       OrganisationUnitService organisationUnitService, Clock clock,
                                       AwardedContractService awardedContractService) {
    this.invitationToTenderParticipantRepository = invitationToTenderParticipantRepository;
    this.organisationUnitService = organisationUnitService;
    this.clock = clock;
    this.awardedContractService = awardedContractService;
  }

  public List<InvitationToTenderParticipant> getInvitationToTenderParticipants(ActualTenderActivity actualTenderActivity) {
    return invitationToTenderParticipantRepository.findAllByActualTenderActivity(actualTenderActivity).stream()
        .sorted(Comparator.comparing(InvitationToTenderParticipant::getCompanyName))
        .toList();
  }

  public List<InvitationToTenderParticipant> getInvitationToTenderParticipantsForActivities(
      List<ActualTenderActivity> activities) {
    return invitationToTenderParticipantRepository.findAllByActualTenderActivityIn(activities);
  }

  public List<InvitationToTenderParticipant> getBidParticipants(ActualTenderActivity actualTenderActivity) {
    return getBidParticipantsFromInvitationToTenderParticipants(getInvitationToTenderParticipants(actualTenderActivity));
  }

  public static List<InvitationToTenderParticipant> getBidParticipantsFromInvitationToTenderParticipants(
      List<InvitationToTenderParticipant> invitationToTenderParticipants) {
    return invitationToTenderParticipants.stream()
        .filter(invitationToTenderParticipant -> Boolean.TRUE.equals(invitationToTenderParticipant.getBidParticipant()))
        .toList();
  }

  public void updateBidParticipants(List<InvitationToTenderParticipant> invitationToTenderParticipants,
                                    List<Integer> bidParticipantIds) {
    invitationToTenderParticipants
        .forEach(participant -> participant.setBidParticipant(bidParticipantIds.contains(participant.getId())));

    invitationToTenderParticipantRepository.saveAll(invitationToTenderParticipants);
  }

  @Transactional
  public void deleteAllByActualTenderActivity(ActualTenderActivity actualTenderActivity) {
    var invitationToTenderParticipants = getInvitationToTenderParticipants(actualTenderActivity);
    invitationToTenderParticipantRepository.deleteAll(invitationToTenderParticipants);
  }

  @Transactional
  public void updateInvitationToTenderParticipants(ActualTenderActivity actualTenderActivity,
                                                   List<String> participantsFromForm) {
    var existingParticipants = getInvitationToTenderParticipants(actualTenderActivity);
    var existingCompanyNames = existingParticipants.stream()
        .map(InvitationToTenderParticipant::getCompanyName)
        .collect(Collectors.toSet());
    var existingCompanyIds = existingParticipants.stream()
        .map(InvitationToTenderParticipant::getOrganisationUnitId)
        .filter(Objects::nonNull)
        .collect(Collectors.toSet());

    var partitionedParticipants = ManualEntryUtil.partitionManualEntries(participantsFromForm);

    // Get the participants that have been removed
    var removedParticipants = existingParticipants.stream()
        .filter(participant -> !partitionedParticipants.ids().contains(participant.getOrganisationUnitId())
            && !partitionedParticipants.manualEntries().contains(participant.getCompanyName()))
        .collect(Collectors.toSet());

    // Create the participants that have been added
    // Participants that are on the energy portal
    var newCompanyIds = partitionedParticipants.ids()
        .stream()
        .filter(companyIdFromForm -> !existingCompanyIds.contains(companyIdFromForm))
        .collect(Collectors.toSet());
    var organisationUnits = organisationUnitService
        .findAllByIds(newCompanyIds.stream().toList(), ORGANISATION_UNIT_REQUEST_PURPOSE);
    var addedIttParticipantsFromEpa = organisationUnits.stream()
        .map(organisationUnit -> {
          var newParticipant = new InvitationToTenderParticipant(
              actualTenderActivity, clock.instant(), organisationUnit.getName());
          newParticipant.setOrganisationUnitId(organisationUnit.getOrganisationUnitId());
          return newParticipant;
        }).collect(Collectors.toSet());

    // Participants that are not on the energy portal
    var addedNonEpaIttParticipants = partitionedParticipants.manualEntries()
        .stream()
        .filter(companyNameFromForm -> !existingCompanyNames.contains(companyNameFromForm))
        .map(newCompanyName -> new InvitationToTenderParticipant(actualTenderActivity, clock.instant(), newCompanyName))
        .collect(Collectors.toSet());

    var newIttParticipants = Stream.concat(addedNonEpaIttParticipants.stream(), addedIttParticipantsFromEpa.stream())
        .collect(Collectors.toSet());

    // Remove the ITT participant from the awarded contract
    var awardedContractOpt = awardedContractService.getByActualTenderActivity(actualTenderActivity);
    awardedContractOpt.ifPresent(
        awardedContract -> {
          if (removedParticipants.contains(awardedContract.getPreferredBidder())) {
            awardedContractService.removePreferredBidder(awardedContract);
          }
        }
    );

    // Delete the removed participants and save the added participants
    invitationToTenderParticipantRepository.deleteAll(removedParticipants);
    invitationToTenderParticipantRepository.saveAll(newIttParticipants);
  }
}
