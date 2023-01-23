package uk.co.nstauthority.scap.scap.casemanagement;

import java.util.List;
import org.springframework.data.repository.CrudRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface CaseEventRepository extends CrudRepository<CaseEvent, Integer> {

  List<CaseEvent> findAllByScapId(Integer scapId);
}
