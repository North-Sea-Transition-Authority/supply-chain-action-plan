package uk.co.nstauthority.scap.scap.actualtender.activity;

import java.util.List;
import org.springframework.data.repository.CrudRepository;

interface InvitationToTenderParticipantRepository extends CrudRepository<InvitationToTenderParticipant, Integer> {

  List<InvitationToTenderParticipant> findAllByActualTenderActivity(ActualTenderActivity actualTenderActivity);
}
