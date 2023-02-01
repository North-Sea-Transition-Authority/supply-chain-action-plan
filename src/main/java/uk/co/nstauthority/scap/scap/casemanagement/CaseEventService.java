package uk.co.nstauthority.scap.scap.casemanagement;

import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventSubject.FURTHER_INFO_REQUESTED;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventSubject.FURTHER_INFO_RESPONSE;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventSubject.SCAP_APPROVED;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventSubject.SCAP_SUBMITTED;

import java.time.Instant;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import uk.co.nstauthority.scap.authentication.UserDetailService;
import uk.co.nstauthority.scap.energyportal.EnergyPortalUserDto;
import uk.co.nstauthority.scap.energyportal.EnergyPortalUserService;
import uk.co.nstauthority.scap.energyportal.WebUserAccountId;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.util.DateUtil;

@Service
public class CaseEventService {

  private final UserDetailService userDetailService;

  private final EnergyPortalUserService energyPortalUserService;

  private final CaseEventRepository caseEventRepository;
  private static final Set<CaseEventSubject> RESPONSE_ACTIONS = Set.of(FURTHER_INFO_RESPONSE,
      SCAP_SUBMITTED,
      SCAP_APPROVED);

  @Autowired
  public CaseEventService(UserDetailService userDetailService,
                          EnergyPortalUserService energyPortalUserService,
                          CaseEventRepository caseEventRepository) {
    this.userDetailService = userDetailService;
    this.energyPortalUserService = energyPortalUserService;
    this.caseEventRepository = caseEventRepository;
  }

  @Transactional
  public CaseEvent recordNewEvent(CaseEventSubject subject,
                                  ScapId scapId,
                                  Integer scapVersion,
                                  String comments) {
    var caseEvent = new CaseEvent();
    caseEvent.setCaseEventSubject(subject);
    caseEvent.setEventTime(Instant.now());
    caseEvent.setScapId(scapId.scapId());
    caseEvent.setVersionNumber(scapVersion);
    caseEvent.setComments(comments);

    var loggedInUser = userDetailService.getUserDetail();
    caseEvent.setEventByWuaId(loggedInUser.getWebUserAccountId().id());

    return caseEventRepository.save(caseEvent);
  }

  public List<CaseEvent> getEventsByScapId(ScapId scapId) {
    return caseEventRepository.findAllByScapId(scapId.scapId());
  }

  public List<CaseEventView> getEventViewByScapId(ScapId scapId) {
    var caseEvents = getEventsByScapId(scapId);

    var users = caseEvents
        .stream()
        .map(CaseEvent::getEventByWuaId)
        .map(WebUserAccountId::new)
        .distinct()
        .toList();

    var userDisplayNames = energyPortalUserService.findByWuaIds(users)
        .stream()
        .collect(Collectors.toMap(EnergyPortalUserDto::webUserAccountId, EnergyPortalUserDto::displayName));

    return caseEvents
        .stream()
        .sorted(Comparator.comparing(CaseEvent::getEventTime).reversed())
        .map(caseEvent ->
            new CaseEventView(
                caseEvent.getTimelineEventSubject().getDisplayName(),
                caseEvent.getScapId(),
                caseEvent.getVersionNumber(),
                DateUtil.instantToString(caseEvent.getEventTime()),
                userDisplayNames.get(caseEvent.getEventByWuaId()),
                caseEvent.getComments(),
                getDateOfResponseToInfoRequest(caseEvent).map(DateUtil::instantToString).orElse(null)))
        .toList();
  }

  public boolean isFurtherInfoResponseOutstanding(ScapId scapId) {
    var requestActionOptional = caseEventRepository
        .findFirstByScapIdAndCaseEventSubjectOrderByEventTimeDesc(scapId.scapId(), FURTHER_INFO_REQUESTED);

    if (requestActionOptional.isPresent()) {
      return getDateOfResponseToInfoRequest(requestActionOptional.get()).isEmpty();
    }
    return false;
  }

  public Optional<Instant> getDateOfResponseToInfoRequest(CaseEvent infoRequest) {
    if (infoRequest.getTimelineEventSubject() != FURTHER_INFO_REQUESTED) {
      return Optional.empty();
    }
    var events = caseEventRepository.findAllByScapIdAndEventTimeAfter(
        infoRequest.getScapId(),
        infoRequest.getEventTime());
    return events.stream()
        .filter(event -> RESPONSE_ACTIONS.contains(event.getTimelineEventSubject()))
        .map(CaseEvent::getEventTime)
        .sorted()
        .findFirst();
  }

  public Set<CaseEventSubject> getApplicableActionsForScap(ScapId scapId) {
    var allActions = Arrays.stream(CaseEventSubject.values()).collect(Collectors.toSet());

    allActions.remove(SCAP_SUBMITTED);
    if (isFurtherInfoResponseOutstanding(scapId)) {
      allActions.remove(FURTHER_INFO_REQUESTED);
    }
    allActions.removeIf(action -> action.getActionPanelId() == null);
    return allActions;
  }
}
