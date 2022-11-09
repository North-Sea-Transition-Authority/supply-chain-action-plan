package uk.co.nstauthority.scap.scap.plannedtender;

import java.util.Optional;
import org.springframework.data.repository.CrudRepository;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;

interface PlannedTenderRepository extends CrudRepository<PlannedTender, Integer> {

  Optional<PlannedTender> findByScapDetail(ScapDetail scapDetail);
}
