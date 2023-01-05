package uk.co.nstauthority.scap.scap.actualtender.activity;

import java.time.Clock;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import javax.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class InvitationToTenderParticipantService {

  private final InvitationToTenderParticipantRepository invitationToTenderParticipantRepository;
  private final Clock clock;

  @Autowired
  InvitationToTenderParticipantService(InvitationToTenderParticipantRepository invitationToTenderParticipantRepository,
                                       Clock clock) {
    this.invitationToTenderParticipantRepository = invitationToTenderParticipantRepository;
    this.clock = clock;
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

    // Get the participants that have been removed
    var removedParticipants = existingParticipants.stream()
        .filter(participant -> !participantsFromForm.contains(participant.getCompanyName()))
        .collect(Collectors.toSet());

    // Create the participants that have been added
    var existingParticipantNames = existingParticipants.stream()
        .map(InvitationToTenderParticipant::getCompanyName)
        .collect(Collectors.toSet());
    var addedParticipants = participantsFromForm.stream()
        .filter(participantName -> !existingParticipantNames.contains(participantName))
        .filter(Objects::nonNull)
        .map(newParticipantName -> new InvitationToTenderParticipant(actualTenderActivity, clock.instant(), newParticipantName))
        .collect(Collectors.toSet());

    // Delete the removed participants and save the added participants
    invitationToTenderParticipantRepository.deleteAll(removedParticipants);
    invitationToTenderParticipantRepository.saveAll(addedParticipants);
  }

}
