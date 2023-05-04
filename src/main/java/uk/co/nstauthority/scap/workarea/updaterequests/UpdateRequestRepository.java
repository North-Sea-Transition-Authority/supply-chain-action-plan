package uk.co.nstauthority.scap.workarea.updaterequests;

import java.util.List;
import java.util.Optional;
import org.springframework.data.repository.CrudRepository;
import uk.co.nstauthority.scap.scap.scap.Scap;

public interface UpdateRequestRepository extends CrudRepository<UpdateRequest, Integer> {
  Optional<UpdateRequest> findFirstByScapAndResolutionDateNullOrderByCreatedTimestampDesc(Scap scap);

  List<UpdateRequest> findAllByScap(Scap scap);

  List<UpdateRequest> findByScapAndUpdateRequestTypeInAndResolutionDateNull(
      Scap scap,
      List<UpdateRequestType> updateRequestTypes);

  Optional<UpdateRequest> findFirstByScapAndResolutionDateNullAndUpdateRequestTypeOrderByCreatedTimestampDesc(
      Scap scap,
      UpdateRequestType updateRequestType);
}
