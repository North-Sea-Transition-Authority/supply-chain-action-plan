package uk.co.nstauthority.scap.application.detail;

import java.time.Clock;
import java.util.Optional;
import javax.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.application.overview.ScapOverview;
import uk.co.nstauthority.scap.error.ScapEntityNotFoundException;

@Service
public class ScapDetailService {

  private final ScapDetailRepository scapDetailRepository;
  private final Clock clock;

  @Autowired
  public ScapDetailService(ScapDetailRepository scapDetailRepository, Clock clock) {
    this.scapDetailRepository = scapDetailRepository;
    this.clock = clock;
  }

  @Transactional
  public void createDraftScapDetail(ScapOverview scap) {
    var isLatestScapDetail = true;
    // TODO SCAP2022-148: replace with actual createdByUserId (1) with actual user ID
    var scapDetail = new ScapDetail(scap, 1, isLatestScapDetail, ScapDetailStatus.DRAFT, clock.instant(), 1);
    scapDetailRepository.save(scapDetail);
  }

  public Optional<ScapDetail> getLatestScapDetailByScap(ScapOverview scap) {
    return scapDetailRepository.findAllByScap(scap)
        .stream()
        .filter(ScapDetail::getTipFlag)
        .findFirst();
  }

  public ScapDetail getLatestScapDetailByScapOrThrow(ScapOverview scap) {
    return getLatestScapDetailByScap(scap).orElseThrow(
        () -> new ScapEntityNotFoundException(
            String.format("Could not find a ScapDetail for Scap with ID [%d]", scap.getId())
        ));
  }
}
