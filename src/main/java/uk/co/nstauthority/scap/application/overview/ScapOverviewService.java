package uk.co.nstauthority.scap.application.overview;

import java.time.Clock;
import javax.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.error.ScapEntityNotFoundException;

@Service
public class ScapOverviewService {

  private final ScapOverviewRepository scapOverviewRepository;
  private final Clock clock;

  @Autowired
  public ScapOverviewService(ScapOverviewRepository scapOverviewRepository, Clock clock) {
    this.scapOverviewRepository = scapOverviewRepository;
    this.clock = clock;
  }

  @Transactional
  public ScapOverview createScapOverview(Integer organisationGroupId) {
    var scapOverview = new ScapOverview(organisationGroupId, clock.instant());
    scapOverviewRepository.save(scapOverview);
    return scapOverview;
  }

  public ScapOverview getScapById(Integer id) {
    return scapOverviewRepository.findById(id)
        .orElseThrow(() ->
            new ScapEntityNotFoundException(String.format("Could not find SCAP overview with id [%d]", id)));
  }

  @Transactional
  public void updateScapOverviewOrganisationGroup(ScapOverview scapOverview, Integer organisationGroupId) {
    scapOverview.setOrganisationGroupId(organisationGroupId);
    scapOverviewRepository.save(scapOverview);
  }
}
