package uk.co.nstauthority.scap.scap.pathfinder;

import jakarta.persistence.EntityManager;
import java.time.Instant;
import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.scap.copy.CopyService;
import uk.co.nstauthority.scap.scap.copy.EntityCopyService;
import uk.co.nstauthority.scap.scap.detail.NewScapType;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;

@Service
public class PathfinderCopyService implements CopyService {

  private final PathfinderService pathfinderService;

  private final PathfinderProjectsOverviewRepository pathfinderOverviewRepository;

  private final EntityCopyService copyService;

  private final EntityManager entityManager;

  public PathfinderCopyService(PathfinderService pathfinderService,
                               PathfinderProjectsOverviewRepository pathfinderOverviewRepository,
                               EntityCopyService copyService, EntityManager entityManager) {
    this.pathfinderService = pathfinderService;
    this.pathfinderOverviewRepository = pathfinderOverviewRepository;
    this.copyService = copyService;
    this.entityManager = entityManager;
  }

  @Override
  public int runOrder() {
    return 70;
  }

  @Override
  public void copyEntity(ScapDetail oldScapDetail, ScapDetail newScapDetail, NewScapType newScapType) {
    var oldPathfinderOverview = pathfinderOverviewRepository.findByScapDetail(oldScapDetail);
    var newPathfinderOverview = BeanUtils.instantiateClass(PathfinderProjectsOverview.class);

    if (oldPathfinderOverview.isPresent()) {
      BeanUtils.copyProperties(oldPathfinderOverview.get(), newPathfinderOverview);
      newPathfinderOverview.setId(null);
      newPathfinderOverview.setScapDetail(newScapDetail);
      newPathfinderOverview.setCreatedTimestamp(Instant.now());
      entityManager.persist(newPathfinderOverview);

      var pathfinderChildren = pathfinderService.findAllByPathfinderProjectsOverview(oldPathfinderOverview.get());
      copyService.copyChildren(newPathfinderOverview, pathfinderChildren);
    }

  }
}
