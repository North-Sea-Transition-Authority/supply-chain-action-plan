package uk.co.nstauthority.scap.application.actualtender.detail;

import java.time.Clock;
import java.util.List;
import javax.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.application.actualtender.ActualTender;

@Service
class ActualTenderDetailService {

  private final ActualTenderDetailRepository actualTenderDetailRepository;
  private final InvitationToTenderParticipantRepository invitationToTenderParticipantRepository;
  private final Clock clock;

  @Autowired
  ActualTenderDetailService(ActualTenderDetailRepository actualTenderDetailRepository,
                            InvitationToTenderParticipantRepository invitationToTenderParticipantRepository,
                            Clock clock) {
    this.actualTenderDetailRepository = actualTenderDetailRepository;
    this.invitationToTenderParticipantRepository = invitationToTenderParticipantRepository;
    this.clock = clock;
  }

  @Transactional
  void createActualTenderDetail(ActualTender actualTender, ActualTenderDetailForm form) {
    var actualTenderDetail = new ActualTenderDetail(actualTender, clock.instant());
    saveActualTenderDetail(actualTenderDetail, form);
  }

  @Transactional
  void updateActualTenderDetail(ActualTenderDetail actualTenderDetail, ActualTenderDetailForm form) {
    //TODO SCAP2022-41: when adding multiple companies, only delete those which have been removed from ITT,
    // and only add those that do not already exist
    var existingInvitationToTenderParticipants = invitationToTenderParticipantRepository
        .findAllByActualTenderDetail(actualTenderDetail);
    invitationToTenderParticipantRepository.deleteAll(existingInvitationToTenderParticipants);
    saveActualTenderDetail(actualTenderDetail, form);
  }

  @Transactional
  void saveActualTenderDetail(ActualTenderDetail actualTenderDetail, ActualTenderDetailForm form) {
    actualTenderDetail.setScopeTitle(form.getScopeTitle().getInputValue());
    actualTenderDetail.setScopeDescription(form.getScopeDescription().getInputValue());
    actualTenderDetail.setRemunerationModel(form.getRemunerationModel());
    actualTenderDetail.setRemunerationModelName(form.getRemunerationModelName().getInputValue());
    actualTenderDetail.setContractStage(form.getContractStage());

    var invitationToTenderParticipant = new InvitationToTenderParticipant(actualTenderDetail, clock.instant());
    invitationToTenderParticipant.setCompanyName(form.getInvitationToTenderParticipants().getInputValue());
    var newInvitationToTenderParticipants = List.of(invitationToTenderParticipant);

    actualTenderDetailRepository.save(actualTenderDetail);
    invitationToTenderParticipantRepository.saveAll(newInvitationToTenderParticipants);
  }
}
