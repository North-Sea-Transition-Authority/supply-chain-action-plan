package uk.co.nstauthority.scap.scap.actualtender.activity;

import java.util.List;
import org.springframework.data.repository.CrudRepository;

interface InvitationToTenderParticipantRepository extends CrudRepository<InvitationToTenderParticipant, Integer> {

  InvitationToTenderParticipant getByOrganisationUnitIdAndCompanyNameAndActualTenderActivity(
      Integer organisationUnitId,
      String companyName,
      ActualTenderActivity actualTenderActivity);

  List<InvitationToTenderParticipant> findAllByActualTenderActivity(ActualTenderActivity actualTenderActivity);

  List<InvitationToTenderParticipant> findAllByActualTenderActivityIn(List<ActualTenderActivity> activities);
}
