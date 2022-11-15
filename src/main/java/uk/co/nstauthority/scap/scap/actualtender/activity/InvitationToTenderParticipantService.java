package uk.co.nstauthority.scap.scap.actualtender.activity;

import java.util.Comparator;
import java.util.List;
import javax.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class InvitationToTenderParticipantService {

  private final InvitationToTenderParticipantRepository invitationToTenderParticipantRepository;

  @Autowired
  InvitationToTenderParticipantService(InvitationToTenderParticipantRepository invitationToTenderParticipantRepository) {
    this.invitationToTenderParticipantRepository = invitationToTenderParticipantRepository;
  }

  public List<InvitationToTenderParticipant> getInvitationToTenderParticipants(ActualTenderActivity actualTenderActivity) {
    return invitationToTenderParticipantRepository.findAllByActualTenderActivity(actualTenderActivity).stream()
        .sorted(Comparator.comparing(InvitationToTenderParticipant::getCompanyName))
        .toList();
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

}
