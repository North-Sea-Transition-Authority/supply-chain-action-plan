package uk.co.nstauthority.scap.scap.projectdetails;

import java.util.Optional;
import org.springframework.data.repository.CrudRepository;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;

interface ProjectDetailsRepository extends CrudRepository<ProjectDetails, Integer> {

  Optional<ProjectDetails> findByScapDetail(ScapDetail scapDetail);
}
