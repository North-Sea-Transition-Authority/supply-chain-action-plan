package uk.co.nstauthority.scap.scap.detail;

import java.time.Clock;
import java.util.Optional;
import javax.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.authentication.UserDetailService;
import uk.co.nstauthority.scap.error.exception.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.scap.scap.Scap;

@Service
public class ScapDetailService {

  private final ScapDetailRepository scapDetailRepository;

  private final UserDetailService userDetailService;
  private final Clock clock;

  @Autowired
  public ScapDetailService(ScapDetailRepository scapDetailRepository, UserDetailService userDetailService, Clock clock) {
    this.scapDetailRepository = scapDetailRepository;
    this.userDetailService = userDetailService;
    this.clock = clock;
  }

  @Transactional
  public void createDraftScapDetail(Scap scap) {
    var isLatestScapDetail = true;
    var userId = userDetailService
        .getUserDetail()
        .wuaId()
        .intValue();

    var scapDetail = new ScapDetail(scap, 1, isLatestScapDetail, ScapDetailStatus.DRAFT, clock.instant(), userId);
    scapDetailRepository.save(scapDetail);
  }

  public Optional<ScapDetail> getLatestScapDetailByScap(Scap scap) {
    return scapDetailRepository.findAllByScap(scap)
        .stream()
        .filter(ScapDetail::getTipFlag)
        .findFirst();
  }

  public ScapDetail getLatestScapDetailByScapOrThrow(Scap scap) {
    return getLatestScapDetailByScap(scap).orElseThrow(
        () -> new ScapEntityNotFoundException(
            String.format("Could not find a ScapDetail for Scap with ID [%d]", scap.getId())
        ));
  }

  public Optional<ScapDetail> getLatestScapDetailByScapId(Integer scapId) {
    return scapDetailRepository.findFirstByScapIdAndTipFlag(scapId, true);
  }

  public ScapDetail getLatestScapDetailByScapIdOrThrow(Integer scapId) {
    return getLatestScapDetailByScapId(scapId).orElseThrow(
        () -> new ScapEntityNotFoundException(
            String.format("Could not find a ScapDetail for Scap with ID [%d]", scapId)
        ));
  }
}
