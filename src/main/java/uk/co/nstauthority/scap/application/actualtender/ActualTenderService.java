package uk.co.nstauthority.scap.application.actualtender;

import java.time.Clock;
import java.util.Optional;
import javax.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.application.detail.ScapDetail;
import uk.co.nstauthority.scap.enumutil.YesNo;
import uk.co.nstauthority.scap.error.ScapEntityNotFoundException;

@Service
public class ActualTenderService {

  private final ActualTenderRepository actualTenderRepository;
  private final Clock clock;

  @Autowired
  ActualTenderService(ActualTenderRepository actualTenderRepository, Clock clock) {
    this.actualTenderRepository = actualTenderRepository;
    this.clock = clock;
  }

  public Optional<ActualTender> getByScapDetail(ScapDetail scapDetail) {
    return actualTenderRepository.findByScapDetail(scapDetail);
  }

  public ActualTender getByScapDetailOrThrow(ScapDetail scapDetail) {
    return getByScapDetail(scapDetail)
        .orElseThrow(() -> new ScapEntityNotFoundException(
            String.format("Could not find actual tender for ScapDetail with ID [%d]", scapDetail.getId())));
  }

  @Transactional
  public void createActualTender(ScapDetail scapDetail, YesNo hasActualTenders) {
    var actualTender = new ActualTender(scapDetail, clock.instant());
    updateHasActualTenders(actualTender, hasActualTenders);
  }

  @Transactional
  public void updateHasActualTenders(ActualTender actualTender, YesNo hasActualTenders) {
    actualTender.setHasActualTenders(YesNo.YES.equals(hasActualTenders));
    actualTenderRepository.save(actualTender);
  }
}
