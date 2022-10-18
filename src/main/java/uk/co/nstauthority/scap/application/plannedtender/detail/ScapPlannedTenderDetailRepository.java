package uk.co.nstauthority.scap.application.plannedtender.detail;

import java.util.List;
import org.springframework.data.repository.CrudRepository;
import uk.co.nstauthority.scap.application.plannedtender.ScapPlannedTender;

public interface ScapPlannedTenderDetailRepository extends CrudRepository<ScapPlannedTenderDetail, Integer> {

  List<ScapPlannedTenderDetail> findAllByPlannedTender(ScapPlannedTender scapPlannedTender);

}
