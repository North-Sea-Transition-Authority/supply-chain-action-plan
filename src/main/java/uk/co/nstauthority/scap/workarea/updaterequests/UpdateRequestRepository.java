package uk.co.nstauthority.scap.workarea.updaterequests;

import java.util.List;
import java.util.Optional;
import org.springframework.data.repository.CrudRepository;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;

public interface UpdateRequestRepository extends CrudRepository<UpdateRequest, Integer> {
  List<UpdateRequest> findByScapDetailAndUpdateRequestTypeInAndResolutionDateNull(
      ScapDetail scapDetail,
      List<UpdateRequestType> updateRequestTypes);

  Optional<UpdateRequest> findFirstByScapDetailAndResolutionDateNullAndUpdateRequestTypeOrderByCreatedTimestampDesc(
      ScapDetail scapDetail,
      UpdateRequestType updateRequestType);
}
