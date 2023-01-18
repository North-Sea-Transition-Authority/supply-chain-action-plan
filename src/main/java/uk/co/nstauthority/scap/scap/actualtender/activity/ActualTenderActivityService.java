package uk.co.nstauthority.scap.scap.actualtender.activity;

import java.time.Clock;
import java.util.Comparator;
import java.util.List;
import javax.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.error.exception.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.scap.RemunerationModel;
import uk.co.nstauthority.scap.scap.actualtender.ActualTender;

@Service
public class ActualTenderActivityService {

  private final ActualTenderActivityRepository actualTenderActivityRepository;
  private final Clock clock;

  @Autowired
  ActualTenderActivityService(ActualTenderActivityRepository actualTenderActivityRepository, Clock clock) {
    this.actualTenderActivityRepository = actualTenderActivityRepository;
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
  void saveActualTenderActivity(ActualTenderActivity actualTenderActivity, ActualTenderActivityForm form) {
    actualTenderActivity.setScopeTitle(form.getScopeTitle().getInputValue());
    actualTenderActivity.setScopeDescription(form.getScopeDescription().getInputValue());
    actualTenderActivity.setRemunerationModel(form.getRemunerationModel());
    if (RemunerationModel.OTHER.equals(form.getRemunerationModel())) {
      actualTenderActivity.setRemunerationModelName(form.getRemunerationModelName().getInputValue());
    } else {
      actualTenderActivity.setRemunerationModelName(null);
    }
    actualTenderActivity.setContractStage(form.getContractStage());

    actualTenderActivityRepository.save(actualTenderActivity);
  }

  @Transactional
  public void deleteActualTenderActivity(ActualTenderActivity actualTenderActivity) {
    actualTenderActivityRepository.delete(actualTenderActivity);
  }
}
