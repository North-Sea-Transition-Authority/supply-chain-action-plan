package uk.co.nstauthority.scap.scap.plannedtender;

import jakarta.transaction.Transactional;
import java.time.Clock;
import java.util.Optional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.error.exception.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;

@Service
public class PlannedTenderService {

  private final PlannedTenderRepository plannedTenderRepository;
  private final Clock clock;

  @Autowired
  public PlannedTenderService(PlannedTenderRepository plannedTenderRepository, Clock clock) {
    this.plannedTenderRepository = plannedTenderRepository;
    this.clock = clock;
  }

  public Optional<PlannedTender> findByScapDetail(ScapDetail scapDetail) {
    return plannedTenderRepository.findByScapDetail(scapDetail);
  }

  public PlannedTender getByScapDetail(ScapDetail scapDetail) {
    return plannedTenderRepository.findByScapDetail(scapDetail)
        .orElseThrow(() -> new ScapEntityNotFoundException(
            String.format("Could not find ScapPlannedTender for ScapDetail with ID [%d]", scapDetail.getId())
        ));
  }

  @Transactional
  public PlannedTender createPlannedTenderForScapDetail(ScapDetail scapDetail) {
    var plannedTender = new PlannedTender(scapDetail, clock.instant());
    plannedTenderRepository.save(plannedTender);
    return plannedTender;
  }

  @Transactional
  public void updatePlannedTenderHasPlannedTenders(PlannedTender plannedTender,
                                                   Boolean status) {
    plannedTender.setHasPlannedTenders(status);
    plannedTenderRepository.save(plannedTender);
  }

  @Transactional
  public void updatePlannedTenderHasMorePlannedTenders(PlannedTender plannedTender,
                                                       HasMorePlannedTenderActivities hasMorePlannedTenderActivities) {
    plannedTender.setHasMorePlannedTenderActivities(hasMorePlannedTenderActivities);
    plannedTenderRepository.save(plannedTender);
  }
}
