package uk.co.nstauthority.scap.scap.projectperformance;

import java.util.Optional;
import org.springframework.data.repository.CrudRepository;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;

interface ProjectPerformanceRepository extends CrudRepository<ProjectPerformance, Integer> {

  Optional<ProjectPerformance> findByScapDetail(ScapDetail scapDetail);
}
