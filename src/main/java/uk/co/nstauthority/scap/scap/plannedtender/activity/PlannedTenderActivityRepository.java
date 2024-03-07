package uk.co.nstauthority.scap.scap.plannedtender.activity;

import java.util.List;
import org.springframework.data.repository.CrudRepository;
import uk.co.nstauthority.scap.scap.plannedtender.PlannedTender;

public interface PlannedTenderActivityRepository extends CrudRepository<PlannedTenderActivity, Integer> {

  List<PlannedTenderActivity> findAllByPlannedTender(PlannedTender plannedTender);

}
