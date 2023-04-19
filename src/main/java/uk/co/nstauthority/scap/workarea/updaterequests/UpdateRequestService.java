package uk.co.nstauthority.scap.workarea.updaterequests;

import java.time.Clock;
import java.time.LocalDate;
import java.util.Arrays;
import java.util.Optional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.authentication.UserDetailService;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEvent;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventSubject;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.scap.ScapId;

@Service
public class UpdateRequestService {

  private final UpdateRequestRepository updateRequestRepository;

  private final ScapDetailService scapDetailService;

  private final UserDetailService userDetailService;

  private final Clock clock;

  @Autowired
  public UpdateRequestService(UpdateRequestRepository updateRequestRepository,
                              ScapDetailService scapDetailService,
                              UserDetailService userDetailService,
                              Clock clock) {
    this.updateRequestRepository = updateRequestRepository;
    this.scapDetailService = scapDetailService;
    this.userDetailService = userDetailService;
    this.clock = clock;
  }

  public UpdateRequest createUpdateRequest(ScapDetail scapDetail,
                                           UpdateRequestType requestType,
                                           LocalDate dueDate,
                                           CaseEvent caseEvent) {
    var updateRequest = new UpdateRequest(scapDetail, requestType, dueDate, caseEvent);
    updateRequest.setCreatedByUserId(userDetailService.getUserDetail().getWebUserAccountId().toInt());
    return updateRequestRepository.save(updateRequest);
  }

  public void resolveUpdateRequest(ScapDetail scapDetail, CaseEventSubject resolvingAction) {
    var updateRequestsResolvedByAction = Arrays.stream(UpdateRequestType.values())
        .filter(type -> type.getResolvedBy().contains(resolvingAction))
        .toList();

    var resolvedRequests = updateRequestRepository
        .findByScapDetailAndUpdateRequestTypeInAndResolutionDateNull(scapDetail, updateRequestsResolvedByAction);

    var userId = userDetailService.getUserDetail().getWebUserAccountId().toInt();

    resolvedRequests.forEach(updateRequest -> {
      updateRequest.setResolutionDate(LocalDate.now(clock));
      updateRequest.setResolvedByUserId(userId);
    });
    updateRequestRepository.saveAll(resolvedRequests);
  }

  public Optional<UpdateRequest> findNextDueUpdate(ScapId scapId) {
    var scapDetail = scapDetailService.findLatestByScapIdAndStatus(scapId, ScapDetailStatus.SUBMITTED);
    if (scapDetail.isEmpty()) {
      return Optional.empty();
    }
    return updateRequestRepository.findFirstByScapDetailAndResolutionDateNullOrderByCreatedTimestampDesc(
        scapDetail.get());
  }

  public Optional<LocalDate> getUpdateDueDate(ScapId scapId, UpdateRequestType requestType) {
    var scapDetail = scapDetailService.findLatestSubmittedScapDetail(scapId);
    if (scapDetail.isEmpty()) {
      return Optional.empty();
    }

    var requestActionOptional = updateRequestRepository
        .findFirstByScapDetailAndResolutionDateNullAndUpdateRequestTypeOrderByCreatedTimestampDesc(scapDetail.get(), requestType);

    return requestActionOptional.map(t -> Optional.of(t.getDueDate())).orElse(Optional.empty());
  }
}
