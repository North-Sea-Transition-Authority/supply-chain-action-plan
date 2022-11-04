package uk.co.nstauthority.scap.application.actualtender.detail;

import java.util.List;
import org.springframework.data.repository.CrudRepository;

interface InvitationToTenderParticipantRepository extends CrudRepository<InvitationToTenderParticipant, Integer> {

  List<InvitationToTenderParticipant> findAllByActualTenderDetail(ActualTenderDetail actualTenderDetail);
}
