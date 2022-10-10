package uk.co.nstauthority.scap.application.overview;

import java.time.Instant;
import javax.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.exception.ScapEntityNotFoundException;

@Service
public class ScapOverviewService {

  private final ScapOverviewRepository scapOverviewRepository;

  @Autowired
  public ScapOverviewService(ScapOverviewRepository scapOverviewRepository) {
    this.scapOverviewRepository = scapOverviewRepository;
  }

  @Transactional
  public ScapOverview createScapOverview(Integer organisationGroupId) {
    var scapOverview = new ScapOverview(organisationGroupId, Instant.now());
    scapOverviewRepository.save(scapOverview);
    return scapOverview;
  }

  public ScapOverview getScapOverviewById(Integer id) {
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
