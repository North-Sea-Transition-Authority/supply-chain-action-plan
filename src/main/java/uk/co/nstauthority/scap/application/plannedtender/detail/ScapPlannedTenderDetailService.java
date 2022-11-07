package uk.co.nstauthority.scap.application.plannedtender.detail;

import java.time.Clock;
import java.util.List;
import javax.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.application.plannedtender.ScapPlannedTender;
import uk.co.nstauthority.scap.error.ScapEntityNotFoundException;

@Service
public class ScapPlannedTenderDetailService {

  private final ScapPlannedTenderDetailRepository scapPlannedTenderDetailRepository;
  private final Clock clock;

  @Autowired
  public ScapPlannedTenderDetailService(ScapPlannedTenderDetailRepository scapPlannedTenderDetailRepository,
                                        Clock clock) {
    this.scapPlannedTenderDetailRepository = scapPlannedTenderDetailRepository;
    this.clock = clock;
  }

  @Transactional
  public void createPlannedTenderDetail(ScapPlannedTender scapPlannedTender, ScapPlannedTenderDetailForm form) {
    var detail = new ScapPlannedTenderDetail(
        scapPlannedTender,
        form.getScopeDescription().getInputValue(),
        form.getEstimatedValue().getAsBigDecimal()
            .orElseThrow(() -> new ClassCastException("Could not cast this forms estimatedValue to BigDecimal")),
        form.getRemunerationModel(),
        form.getRemunerationModelName().getInputValue(),
        form.getAwardRationale().getInputValue(),
        clock.instant()
    );
    scapPlannedTenderDetailRepository.save(detail);
  }

  public List<ScapPlannedTenderDetail> getTenderDetailsByPlannedTender(ScapPlannedTender scapPlannedTender) {
    return scapPlannedTenderDetailRepository.findAllByPlannedTender(scapPlannedTender);
  }

  public boolean hasExistingTenderDetails(ScapPlannedTender scapPlannedTender) {
    return !getTenderDetailsByPlannedTender(scapPlannedTender).isEmpty();
  }

  public ScapPlannedTenderDetail getPlannedTenderDetailById(Integer id) {
    return scapPlannedTenderDetailRepository.findById(id)
        .orElseThrow(() -> new ScapEntityNotFoundException(
            String.format("Could not find Planned Tender Detail with id %d", id)
        ));
  }

  @Transactional
  public void deletePlannedTenderDetail(ScapPlannedTenderDetail detail) {
    scapPlannedTenderDetailRepository.delete(detail);
  }

  @Transactional
  public void updatePlannedTenderDetail(ScapPlannedTenderDetail plannedTenderDetail, ScapPlannedTenderDetailForm form) {
    var estimatedValue = form.getEstimatedValue().getAsBigDecimal().orElseThrow(() ->
        new ClassCastException(
            String.format("Could not update planned tender detail with ID %d, as estimatedValue is not a BigDecimal",
                plannedTenderDetail.getId())));

    plannedTenderDetail.setAwardRationale(form.getAwardRationale().getInputValue());
    plannedTenderDetail.setEstimatedValue(estimatedValue);
    plannedTenderDetail.setRemunerationModel(form.getRemunerationModel());
    plannedTenderDetail.setRemunerationModelName(form.getRemunerationModelName().getInputValue());
    plannedTenderDetail.setScopeDescription(form.getScopeDescription().getInputValue());
    scapPlannedTenderDetailRepository.save(plannedTenderDetail);
  }
}
