package uk.co.nstauthority.scap.scap.casemanagement;

import java.time.Instant;
import java.util.List;
import java.util.Optional;
import org.springframework.data.repository.CrudRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface CaseEventRepository extends CrudRepository<CaseEvent, Integer> {

  List<CaseEvent> findAllByScapId(Integer scapId);

  List<CaseEvent> findAllByScapIdAndEventTimeAfter(Integer scapId, Instant eventTime);

  Optional<CaseEvent> findFirstByScapIdAndCaseEventSubjectOrderByEventTimeDesc(Integer scapId,
                                                                               CaseEventSubject caseEventSubject);
}
