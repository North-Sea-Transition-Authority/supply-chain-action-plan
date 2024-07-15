package uk.co.nstauthority.scap.scap.pathfinder;

import jakarta.transaction.Transactional;
import java.time.Clock;
import java.util.Collections;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.energyportal.PathfinderProjectService;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;

@Service
public class PathfinderService {

  private final PathfinderProjectRepository pathfinderProjectRepository;
  private final PathfinderProjectsOverviewRepository pathfinderProjectsOverviewRepository;
  private final PathfinderProjectService pathfinderProjectService;
  private final Clock clock;

  @Autowired
  PathfinderService(PathfinderProjectRepository pathfinderProjectRepository,
                    PathfinderProjectsOverviewRepository pathfinderProjectsOverviewRepository,
                    PathfinderProjectService pathfinderProjectService,
                    Clock clock) {
    this.pathfinderProjectRepository = pathfinderProjectRepository;
    this.pathfinderProjectsOverviewRepository = pathfinderProjectsOverviewRepository;
    this.pathfinderProjectService = pathfinderProjectService;
    this.clock = clock;
  }

  @Transactional
  public void saveRelatedPathfinderProjects(ScapDetail scapDetail,
                                            PathfinderForm form) {
    var pathfinderProjectsOverviewOptional = findPathfinderProjectsOverview(scapDetail);
    var pathfinderProjectsOverview = pathfinderProjectsOverviewOptional
        .orElse(new PathfinderProjectsOverview(scapDetail, clock.instant()));
    pathfinderProjectsOverview.update(form);

    pathfinderProjectsOverviewRepository.save(pathfinderProjectsOverview);

    var existingPathfinderProjects = pathfinderProjectsOverviewOptional
        .map(this::findAllByPathfinderProjectsOverview)
        .orElse(Collections.emptySet());

    var newPathfinderProjects = pathfinderProjectService.getPathfinderProjectsByIds(form.getPathfinderProjectIds())
        .stream()
        .map(pathfinderProject -> new PathfinderProject(
            pathfinderProjectsOverview,
            pathfinderProject.getProjectId(),
            pathfinderProject.getProjectTitle(),
            clock.instant()
        ))
        .collect(Collectors.toSet());

    pathfinderProjectRepository.deleteAll(existingPathfinderProjects);
    pathfinderProjectRepository.saveAll(newPathfinderProjects);
  }

  public Optional<PathfinderProjectsOverview> findPathfinderProjectsOverview(ScapDetail scapDetail) {
    return pathfinderProjectsOverviewRepository.findByScapDetail(scapDetail);
  }

  public Set<PathfinderProject> findAllByPathfinderProjectsOverview(PathfinderProjectsOverview pathfinderProjectsOverview) {
    if (Boolean.FALSE.equals(pathfinderProjectsOverview.getHasRelatedPathfinderProjects())) {
      return Collections.emptySet();
    }

    return pathfinderProjectRepository.findAllByPathfinderProjectsOverview(pathfinderProjectsOverview);
  }
}
