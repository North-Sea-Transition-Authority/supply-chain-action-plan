package uk.co.nstauthority.scap.scap.scap;

import java.time.Clock;
import javax.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.error.ScapEntityNotFoundException;

@Service
public class ScapService {

  private final ScapRepository scapRepository;
  private final Clock clock;

  @Autowired
  public ScapService(ScapRepository scapRepository, Clock clock) {
    this.scapRepository = scapRepository;
    this.clock = clock;
  }

  @Transactional
  public Scap createScapOverview(Integer organisationGroupId) {
    var scapOverview = new Scap(organisationGroupId, clock.instant());
    scapRepository.save(scapOverview);
    return scapOverview;
  }

  public Scap getScapById(Integer id) {
    return scapRepository.findById(id)
        .orElseThrow(() ->
            new ScapEntityNotFoundException(String.format("Could not find SCAP overview with id [%d]", id)));
  }

  @Transactional
  public void updateScapOverviewOrganisationGroup(Scap scap, Integer organisationGroupId) {
    scap.setOrganisationGroupId(organisationGroupId);
    scapRepository.save(scap);
  }
}
