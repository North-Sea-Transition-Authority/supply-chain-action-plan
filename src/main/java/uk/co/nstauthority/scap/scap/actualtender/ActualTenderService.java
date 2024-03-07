package uk.co.nstauthority.scap.scap.actualtender;

import java.time.Clock;
import java.util.Optional;
import javax.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.enumutil.YesNo;
import uk.co.nstauthority.scap.error.exception.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.scap.actualtender.summary.HasMoreActualTenderActivities;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;

@Service
public class ActualTenderService {

  private final ActualTenderRepository actualTenderRepository;
  private final Clock clock;

  @Autowired
  ActualTenderService(ActualTenderRepository actualTenderRepository, Clock clock) {
    this.actualTenderRepository = actualTenderRepository;
    this.clock = clock;
  }

  public Optional<ActualTender> findByScapDetail(ScapDetail scapDetail) {
    return actualTenderRepository.findByScapDetail(scapDetail);
  }

  public ActualTender getByScapDetail(ScapDetail scapDetail) {
    return findByScapDetail(scapDetail)
        .orElseThrow(() -> new ScapEntityNotFoundException(
            String.format("Could not find actual tender for ScapDetail with ID [%d]", scapDetail.getId())));
  }

  @Transactional
  public void createActualTender(ScapDetail scapDetail, YesNo hasActualTenders) {
    var actualTender = new ActualTender(scapDetail, clock.instant());
    updateHasActualTenders(actualTender, YesNo.YES.equals(hasActualTenders));
  }

  @Transactional
  public void updateHasActualTenders(ActualTender actualTender, Boolean hasActualTenders) {
    actualTender.setHasActualTenders(hasActualTenders);
    actualTenderRepository.save(actualTender);
  }

  @Transactional
  public void updateHasMoreActualTenders(ActualTender actualTender,
                                         HasMoreActualTenderActivities hasMoreActualTenderActivities) {
    actualTender.setHasMoreActualTenders(hasMoreActualTenderActivities);
    actualTenderRepository.save(actualTender);
  }
}
