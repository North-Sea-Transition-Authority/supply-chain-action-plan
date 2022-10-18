package uk.co.nstauthority.scap.application.plannedtender.detail;

import java.time.Instant;
import java.util.List;
import javax.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.application.plannedtender.ScapPlannedTender;
import uk.co.nstauthority.scap.error.ScapEntityNotFoundException;

@Service
public class ScapPlannedTenderDetailService {

  private final ScapPlannedTenderDetailRepository scapPlannedTenderDetailRepository;

  @Autowired
  public ScapPlannedTenderDetailService(ScapPlannedTenderDetailRepository scapPlannedTenderDetailRepository) {
    this.scapPlannedTenderDetailRepository = scapPlannedTenderDetailRepository;
  }

  @Transactional
  public void createPlannedTenderDetail(ScapPlannedTender scapPlannedTender, ScapPlannedTenderDetailForm form) {
    var detail = new ScapPlannedTenderDetail(
        scapPlannedTender,
        form.getScopeDescription().getInputValue(),
        form.getEstimatedValue().getInputValueAsBigDecimal()
            .orElseThrow(() -> new ClassCastException("Could not cast this forms estimatedValue to BigDecimal")),
        form.getRemunerationModel(),
        form.getRemunerationModelName().getInputValue(),
        form.getAwardRationale().getInputValue(),
        Instant.now()
    );
    scapPlannedTenderDetailRepository.save(detail);
  }

  public List<ScapPlannedTenderDetail> getTenderDetailsByPlannedTender(ScapPlannedTender scapPlannedTender) {
    return scapPlannedTenderDetailRepository.findAllByPlannedTender(scapPlannedTender);
  }

  public Boolean hasExistingTenderDetails(ScapPlannedTender scapPlannedTender) {
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
}
