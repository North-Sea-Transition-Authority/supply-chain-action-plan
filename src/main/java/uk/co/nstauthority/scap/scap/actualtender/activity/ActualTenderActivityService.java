package uk.co.nstauthority.scap.scap.actualtender.activity;

import java.time.Clock;
import java.util.Comparator;
import java.util.List;
import javax.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.error.exception.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.scap.actualtender.ActualTender;

@Service
public class ActualTenderActivityService {

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

  public ActualTenderActivity getById(Integer id) {
    return actualTenderActivityRepository.findById(id).orElseThrow(
        () -> new ScapEntityNotFoundException(
            String.format("Could not find actual tender activity with ID [%s]", id))
    );
  }

  public List<ActualTenderActivity> getAllByActualTender(ActualTender actualTender) {
    return actualTenderActivityRepository.findAllByActualTender(actualTender)
        .stream()
        .sorted(Comparator.comparing(ActualTenderActivity::getScopeTitle))
        .toList();
  }

  public List<ActualTenderActivity> getActivitiesWithContractAwarded(ActualTender actualTender) {
    return actualTenderActivityRepository
        .findAllByActualTenderAndContractStage(actualTender, ContractStage.CONTRACT_AWARDED)
        .stream()
        .sorted(Comparator.comparing(ActualTenderActivity::getScopeTitle))
        .toList();
  }

  public boolean hasActualTenderActivity(ActualTender actualTender) {
    return actualTenderActivityRepository.findFirstByActualTender(actualTender).isPresent();
  }

  @Transactional
  ActualTenderActivity createActualTenderActivity(ActualTender actualTender, ActualTenderActivityForm form) {
    var actualTenderActivity = new ActualTenderActivity(actualTender, clock.instant());
    saveActualTenderActivity(actualTenderActivity, form);
    return actualTenderActivity;
  }

  @Transactional
  void updateActualTenderActivity(ActualTenderActivity actualTenderActivity, ActualTenderActivityForm form) {
    //TODO SCAP2022-41: when adding multiple companies, only delete those which have been removed from ITT,
    // and only add those that do not already exist
    saveActualTenderActivity(actualTenderActivity, form);
  }

  @Transactional
  void saveActualTenderActivity(ActualTenderActivity actualTenderActivity, ActualTenderActivityForm form) {
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

  @Transactional
  public void deleteActualTenderActivity(ActualTenderActivity actualTenderActivity) {
    actualTenderActivityRepository.delete(actualTenderActivity);
  }
}
