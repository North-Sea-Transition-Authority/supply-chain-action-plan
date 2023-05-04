package uk.co.nstauthority.scap.scap.pathfinder;

import java.util.Set;
import java.util.UUID;
import org.springframework.data.repository.CrudRepository;

interface PathfinderProjectRepository extends CrudRepository<PathfinderProject, UUID> {

  Set<PathfinderProject> findAllByPathfinderProjectsOverview(PathfinderProjectsOverview pathfinderProjectsOverview);

}
