package uk.co.nstauthority.scap.scap.actualtender.activity.awardedcontract;

import java.util.Optional;
import org.springframework.data.repository.CrudRepository;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivity;

interface AwardedContractRepository extends CrudRepository<AwardedContract, Integer> {

  Optional<AwardedContract> findByActualTenderActivity(ActualTenderActivity actualTenderActivity);
}
