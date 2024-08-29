package uk.co.nstauthority.scap.workarea.updaterequests;

import java.time.Clock;
import java.time.LocalDate;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import javax.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.authentication.UserDetailService;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEvent;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventSubject;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.scap.ScapService;

@Service
public class UpdateRequestService {

  private final UpdateRequestRepository updateRequestRepository;

  private final ScapService scapService;

  private final UserDetailService userDetailService;

  private final Clock clock;

  @Autowired
  public UpdateRequestService(UpdateRequestRepository updateRequestRepository,
                              ScapService scapService,
                              UserDetailService userDetailService,
                              Clock clock) {
    this.updateRequestRepository = updateRequestRepository;
    this.scapService = scapService;
    this.userDetailService = userDetailService;
    this.clock = clock;
  }

  public void createUpdateRequest(ScapDetail scapDetail,
                                           UpdateRequestType requestType,
                                           LocalDate dueDate,
                                           CaseEvent caseEvent) {
    var updateRequest = new UpdateRequest(scapDetail.getScap(), requestType, dueDate, caseEvent);
    updateRequest.setCreatedTimestamp(LocalDate.now());
    updateRequest.setCreatedByUserId(userDetailService.getUserDetail().getWebUserAccountId().toInt());
    updateRequestRepository.save(updateRequest);
  }

  @Transactional
  public void resolveUpdateRequest(ScapDetail scapDetail, CaseEventSubject resolvingAction) {
    var updateRequestsResolvedByAction = Arrays.stream(UpdateRequestType.values())
        .filter(type -> type.getResolvedBy().contains(resolvingAction))
        .toList();

    var resolvedRequests = updateRequestRepository
        .findByScapAndUpdateRequestTypeInAndResolutionDateNull(scapDetail.getScap(), updateRequestsResolvedByAction);

    var userId = userDetailService.getUserDetail().getWebUserAccountId().toInt();

    resolvedRequests.forEach(updateRequest -> {
      updateRequest.setResolutionDate(LocalDate.now(clock));
      updateRequest.setResolvedByUserId(userId);
    });
    updateRequestRepository.saveAll(resolvedRequests);
  }

  public Optional<UpdateRequest> findNextDueUpdate(ScapId scapId) {
    return updateRequestRepository.findFirstByScapAndResolutionDateNullOrderByCreatedTimestampDesc(
        scapService.getScapById(scapId));
  }

  public Optional<LocalDate> getUpdateDueDate(ScapId scapId, UpdateRequestType requestType) {
    var requestActionOptional = updateRequestRepository
        .findFirstByScapAndResolutionDateNullAndUpdateRequestTypeOrderByCreatedTimestampDesc(
            scapService.getScapById(scapId),
            requestType);

    return requestActionOptional.map(UpdateRequest::getDueDate);
  }

  public List<UpdateRequest> findAllByScapId(ScapId scapId) {
    var scap = scapService.getScapById(scapId);
    return updateRequestRepository.findAllByScap(scap);
  }
}
