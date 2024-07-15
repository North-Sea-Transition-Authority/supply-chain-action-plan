package uk.co.nstauthority.scap.scap.plannedtender.activity;

import jakarta.transaction.Transactional;
import java.time.Clock;
import java.util.List;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.error.exception.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.scap.RemunerationModel;
import uk.co.nstauthority.scap.scap.plannedtender.PlannedTender;

@Service
public class PlannedTenderActivityService {

  private final PlannedTenderActivityRepository plannedTenderActivityRepository;
  private final Clock clock;

  @Autowired
  public PlannedTenderActivityService(PlannedTenderActivityRepository plannedTenderActivityRepository,
                                      Clock clock) {
    this.plannedTenderActivityRepository = plannedTenderActivityRepository;
    this.clock = clock;
  }

  @Transactional
  public void createPlannedTenderDetail(PlannedTender plannedTender, PlannedTenderActivityForm form) {
    var activity = new PlannedTenderActivity(plannedTender, clock.instant()
    );
    updatePlannedTenderDetail(activity, form);
  }

  public List<PlannedTenderActivity> getTenderDetailsByPlannedTender(PlannedTender plannedTender) {
    return plannedTenderActivityRepository.findAllByPlannedTender(plannedTender);
  }

  public boolean hasExistingTenderDetails(PlannedTender plannedTender) {
    return !getTenderDetailsByPlannedTender(plannedTender).isEmpty();
  }

  public PlannedTenderActivity getPlannedTenderDetailById(Integer id) {
    return plannedTenderActivityRepository.findById(id)
        .orElseThrow(() -> new ScapEntityNotFoundException(
            String.format("Could not find Planned Tender Detail with id %d", id)
        ));
  }

  @Transactional
  public void deletePlannedTenderDetail(PlannedTenderActivity detail) {
    plannedTenderActivityRepository.delete(detail);
  }

  @Transactional
  public void updatePlannedTenderDetail(PlannedTenderActivity plannedTenderActivity, PlannedTenderActivityForm form) {
    var estimatedValue = form.getEstimatedValue().getAsBigDecimal().orElseThrow(() ->
        new ClassCastException(
            String.format("Could not update planned tender detail with ID %d, as estimatedValue is not a BigDecimal",
                plannedTenderActivity.getId())));

    plannedTenderActivity.setAwardRationale(form.getAwardRationale().getInputValue());
    plannedTenderActivity.setEstimatedValue(estimatedValue);
    plannedTenderActivity.setRemunerationModel(form.getRemunerationModel());
    if (RemunerationModel.OTHER.equals(form.getRemunerationModel())) {
      plannedTenderActivity.setRemunerationModelName(form.getRemunerationModelName().getInputValue());
    } else {
      plannedTenderActivity.setRemunerationModelName(null);
    }
    plannedTenderActivity.setScopeDescription(form.getScopeDescription().getInputValue());
    var expectedActualTenderStartDate = form.getIndicativeActualTenderStartDate()
        .getAsLocalDate()
        .orElseThrow(() -> new ClassCastException(
            "Update failed for planned tender activity with ID %d, as indicativeActualTenderStartDate is not a LocalDate"
                .formatted(plannedTenderActivity.getId())));
    plannedTenderActivity.setExpectedActualTenderStartDate(expectedActualTenderStartDate);

    var expectedContractAwardDate = form.getIndicativeContractAwardDate()
        .getAsLocalDate()
        .orElseThrow(() -> new ClassCastException(
            "Update failed for planned tender activity with ID %d, as indicativeContractAwardDate is not a LocalDate"
                .formatted(plannedTenderActivity.getId())));

    plannedTenderActivity.setExpectedContractAwardDate(expectedContractAwardDate);

    plannedTenderActivityRepository.save(plannedTenderActivity);
  }
}
