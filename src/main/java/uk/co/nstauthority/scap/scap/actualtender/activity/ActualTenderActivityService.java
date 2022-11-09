package uk.co.nstauthority.scap.scap.actualtender.activity;

import java.time.Clock;
import java.util.List;
import javax.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.scap.actualtender.ActualTender;

@Service
class ActualTenderActivityService {

  private final ActualTenderActivityRepository actualTenderActivityRepository;
  private final InvitationToTenderParticipantRepository invitationToTenderParticipantRepository;
  private final Clock clock;

  @Autowired
  ActualTenderActivityService(ActualTenderActivityRepository actualTenderActivityRepository,
                              InvitationToTenderParticipantRepository invitationToTenderParticipantRepository,
                              Clock clock) {
    this.actualTenderActivityRepository = actualTenderActivityRepository;
    this.invitationToTenderParticipantRepository = invitationToTenderParticipantRepository;
    this.clock = clock;
  }

  @Transactional
  void createActualTenderDetail(ActualTender actualTender, ActualTenderActivityForm form) {
    var actualTenderDetail = new ActualTenderActivity(actualTender, clock.instant());
    saveActualTenderDetail(actualTenderDetail, form);
  }

  @Transactional
  void updateActualTenderDetail(ActualTenderActivity actualTenderActivity, ActualTenderActivityForm form) {
    //TODO SCAP2022-41: when adding multiple companies, only delete those which have been removed from ITT,
    // and only add those that do not already exist
    var existingInvitationToTenderParticipants = invitationToTenderParticipantRepository
        .findAllByActualTenderActivity(actualTenderActivity);
    invitationToTenderParticipantRepository.deleteAll(existingInvitationToTenderParticipants);
    saveActualTenderDetail(actualTenderActivity, form);
  }

  @Transactional
  void saveActualTenderDetail(ActualTenderActivity actualTenderActivity, ActualTenderActivityForm form) {
    actualTenderActivity.setScopeTitle(form.getScopeTitle().getInputValue());
    actualTenderActivity.setScopeDescription(form.getScopeDescription().getInputValue());
    actualTenderActivity.setRemunerationModel(form.getRemunerationModel());
    actualTenderActivity.setRemunerationModelName(form.getRemunerationModelName().getInputValue());
    actualTenderActivity.setContractStage(form.getContractStage());

    var invitationToTenderParticipant = new InvitationToTenderParticipant(actualTenderActivity, clock.instant());
    invitationToTenderParticipant.setCompanyName(form.getInvitationToTenderParticipants().getInputValue());
    var newInvitationToTenderParticipants = List.of(invitationToTenderParticipant);

    actualTenderActivityRepository.save(actualTenderActivity);
    invitationToTenderParticipantRepository.saveAll(newInvitationToTenderParticipants);
  }
}
