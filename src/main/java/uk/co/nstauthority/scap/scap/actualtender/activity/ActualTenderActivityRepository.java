package uk.co.nstauthority.scap.scap.actualtender.activity;

import java.util.List;
import java.util.Optional;
import org.springframework.data.repository.CrudRepository;
import uk.co.nstauthority.scap.scap.actualtender.ActualTender;

interface ActualTenderActivityRepository extends CrudRepository<ActualTenderActivity, Integer> {

  List<ActualTenderActivity> findAllByActualTender(ActualTender actualTender);

  Optional<ActualTenderActivity> findFirstByActualTender(ActualTender actualTender);
}
