package uk.co.nstauthority.scap.scap.actualtender.activity;

import java.util.Comparator;
import java.util.List;
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

  public void updateBidParticipants(List<InvitationToTenderParticipant> invitationToTenderParticipants,
                                    List<Integer> bidParticipantIds) {
    invitationToTenderParticipants
        .forEach(participant -> participant.setBidParticipant(bidParticipantIds.contains(participant.getId())));

    invitationToTenderParticipantRepository.saveAll(invitationToTenderParticipants);
  }

}
