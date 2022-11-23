package uk.co.nstauthority.scap.scap.plannedtender.activity;

import java.time.Clock;
import java.util.List;
import javax.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.error.exception.ScapEntityNotFoundException;
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
    var activity = new PlannedTenderActivity(
        plannedTender,
        form.getScopeDescription().getInputValue(),
        form.getEstimatedValue().getAsBigDecimal()
            .orElseThrow(() -> new ClassCastException("Could not cast this forms estimatedValue to BigDecimal")),
        form.getRemunerationModel(),
        form.getRemunerationModelName().getInputValue(),
        form.getAwardRationale().getInputValue(),
        clock.instant()
    );
    plannedTenderActivityRepository.save(activity);
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
  public void updatePlannedTenderDetail(PlannedTenderActivity plannedTenderDetail, PlannedTenderActivityForm form) {
    var estimatedValue = form.getEstimatedValue().getAsBigDecimal().orElseThrow(() ->
        new ClassCastException(
            String.format("Could not update planned tender detail with ID %d, as estimatedValue is not a BigDecimal",
                plannedTenderDetail.getId())));

    plannedTenderDetail.setAwardRationale(form.getAwardRationale().getInputValue());
    plannedTenderDetail.setEstimatedValue(estimatedValue);
    plannedTenderDetail.setRemunerationModel(form.getRemunerationModel());
    plannedTenderDetail.setRemunerationModelName(form.getRemunerationModelName().getInputValue());
    plannedTenderDetail.setScopeDescription(form.getScopeDescription().getInputValue());
    plannedTenderActivityRepository.save(plannedTenderDetail);
  }
}
