package uk.co.nstauthority.scap.scap.pathfinder;

import java.util.Optional;
import java.util.UUID;
import org.springframework.data.repository.CrudRepository;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;

interface PathfinderProjectsOverviewRepository extends CrudRepository<PathfinderProjectsOverview, UUID> {

  Optional<PathfinderProjectsOverview> findByScapDetail(ScapDetail scapDetail);

}
