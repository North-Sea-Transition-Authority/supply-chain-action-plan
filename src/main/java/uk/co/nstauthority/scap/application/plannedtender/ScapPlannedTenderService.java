package uk.co.nstauthority.scap.application.plannedtender;

import java.time.Instant;
import java.util.Optional;
import javax.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.application.detail.ScapDetail;
import uk.co.nstauthority.scap.error.ScapEntityNotFoundException;

@Service
public class ScapPlannedTenderService {

  private final ScapPlannedTenderRepository scapPlannedTenderRepository;

  @Autowired
  public ScapPlannedTenderService(ScapPlannedTenderRepository scapPlannedTenderRepository) {
    this.scapPlannedTenderRepository = scapPlannedTenderRepository;
  }

  public Optional<ScapPlannedTender> getScapPlannedTenderByScapDetail(ScapDetail scapDetail) {
    return scapPlannedTenderRepository.findByScapDetail(scapDetail);
  }

  public ScapPlannedTender getScapPlannedTenderByScapDetailOrThrow(ScapDetail scapDetail) {
    return scapPlannedTenderRepository.findByScapDetail(scapDetail)
        .orElseThrow(() -> new ScapEntityNotFoundException(
            String.format("Could not find ScapPlannedTender for ScapDetail with ID [%d]", scapDetail.getId())
        ));
  }

  @Transactional
  public ScapPlannedTender createPlannedTenderForScapDetail(ScapDetail scapDetail) {
    var plannedTender = new ScapPlannedTender(scapDetail, Instant.now());
    scapPlannedTenderRepository.save(plannedTender);
    return plannedTender;
  }

  @Transactional
  public void updatePlannedTenderHasPlannedTenders(ScapPlannedTender plannedTender,
                                                   Boolean status) {
    plannedTender.setHasPlannedTenders(status);
    scapPlannedTenderRepository.save(plannedTender);
  }

  @Transactional
  public void updatePlannedTenderHasMorePlannedTenders(ScapPlannedTender plannedTender,
                                                       HasMorePlannedTenderActivities hasMorePlannedTenderActivities) {
    plannedTender.setHasMorePlannedTenderActivities(hasMorePlannedTenderActivities);
    scapPlannedTenderRepository.save(plannedTender);
  }
}
