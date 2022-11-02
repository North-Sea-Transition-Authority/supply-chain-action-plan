package uk.co.nstauthority.scap.application.actualtender;

import java.time.Instant;
import java.util.Optional;
import javax.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.application.detail.ScapDetail;
import uk.co.nstauthority.scap.enumutil.YesNo;

@Service
public class ActualTenderService {

  private final ActualTenderRepository actualTenderRepository;

  @Autowired
  ActualTenderService(ActualTenderRepository actualTenderRepository) {
    this.actualTenderRepository = actualTenderRepository;
  }

  public Optional<ActualTender> getByScapDetail(ScapDetail scapDetail) {
    return actualTenderRepository.findByScapDetail(scapDetail);
  }

  @Transactional
  public void createActualTender(ScapDetail scapDetail, YesNo hasActualTenders) {
    var actualTender = new ActualTender(scapDetail, Instant.now());
    updateHasActualTenders(actualTender, hasActualTenders);
  }

  @Transactional
  public void updateHasActualTenders(ActualTender actualTender, YesNo hasActualTenders) {
    actualTender.setHasActualTenders(YesNo.YES.equals(hasActualTenders));
    actualTenderRepository.save(actualTender);
  }
}
