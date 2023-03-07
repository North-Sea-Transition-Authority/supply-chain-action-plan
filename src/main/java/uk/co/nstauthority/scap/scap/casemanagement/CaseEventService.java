package uk.co.nstauthority.scap.scap.casemanagement;

import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventSubject.FURTHER_INFO_REQUESTED;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventSubject.FURTHER_INFO_RESPONSE;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventSubject.QA_COMMENT;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventSubject.SCAP_APPROVED;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventSubject.SCAP_CONSULTATION_REQUESTED;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventSubject.SCAP_CONSULTATION_RESPONSE;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventSubject.SCAP_REINSTATED;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventSubject.SCAP_SUBMITTED;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventSubject.SCAP_WITHDRAWN;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventSubject.UPDATE_SCAP;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
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
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.util.DateUtil;

@Service
public class CaseEventService {

  private final UserDetailService userDetailService;

  private final EnergyPortalUserService energyPortalUserService;

  private final CaseEventRepository caseEventRepository;

  private final TeamService teamService;

  private final ScapDetailService scapDetailService;

  private static final String CONSULTATIONS = "Consultations";
  private static final String UPDATE_SCAP = "UPDATE_SCAP";
  private static final String DECISIONS = "Decisions";
  private static final String FURTHER_INFO = "Further Info";
  private static final String QA = "QA";
  private static final Set<CaseEventSubject> RESPONSE_ACTIONS = Set.of(FURTHER_INFO_RESPONSE,
      SCAP_SUBMITTED,
      SCAP_APPROVED);

  private static final Set<ScapDetailStatus> TERMINAL_DETAIL_STATUS = Set.of(ScapDetailStatus.CLOSED_OUT,
      ScapDetailStatus.WITHDRAWN);

  @Autowired
  public CaseEventService(UserDetailService userDetailService,
                          EnergyPortalUserService energyPortalUserService,
                          CaseEventRepository caseEventRepository,
                          TeamService teamService,
                          ScapDetailService scapDetailService) {
    this.userDetailService = userDetailService;
    this.energyPortalUserService = energyPortalUserService;
    this.caseEventRepository = caseEventRepository;
    this.teamService = teamService;
    this.scapDetailService = scapDetailService;
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

  public Map<String, List<CaseEventSubject>> getApplicableActionsForScap(ScapId scapId) {
    var user = userDetailService.getUserDetail();
    var scapDetail = scapDetailService.getLatestScapDetailByScapIdOrThrow(scapId);

    var actionMap = new LinkedHashMap<String, List<CaseEventSubject>>();
    if (teamService.userIsMemberOfRegulatorTeam(user)) {
      actionMap.putAll(getRegulatorTeamActions(scapDetail));
    } else {
      if (TERMINAL_DETAIL_STATUS.contains(scapDetail.getStatus())) {
        return Collections.emptyMap();
      }
      var furtherInfo = new ArrayList<CaseEventSubject>();
      var consultations = new ArrayList<CaseEventSubject>();
      if (isFurtherInfoResponseOutstanding(scapId)) {
        furtherInfo.add(FURTHER_INFO_RESPONSE);
      }
      consultations.add(SCAP_CONSULTATION_RESPONSE);
      actionMap.put(FURTHER_INFO, furtherInfo);
      actionMap.put(CONSULTATIONS, consultations);
      actionMap.put(UPDATE_SCAP, Collections.emptyList());
    }
    return actionMap;
  }

  private Map<String, List<CaseEventSubject>> getRegulatorTeamActions(ScapDetail scapDetail) {
    if (TERMINAL_DETAIL_STATUS.contains(scapDetail.getStatus())) {
      var regulatorMap = new HashMap<String, List<CaseEventSubject>>();
      var descisions = new ArrayList<CaseEventSubject>();
      descisions.add(SCAP_REINSTATED);
      regulatorMap.put(DECISIONS, descisions);
      return regulatorMap;
    }
    var regulatorMap = new HashMap<String, List<CaseEventSubject>>();
    var qa = new ArrayList<CaseEventSubject>();
    qa.add(QA_COMMENT);

    var consultations = new ArrayList<CaseEventSubject>();
    consultations.add(SCAP_CONSULTATION_REQUESTED);
    consultations.add(SCAP_CONSULTATION_RESPONSE);

    var descisions = new ArrayList<CaseEventSubject>();
    descisions.add(SCAP_APPROVED);
    descisions.add(SCAP_WITHDRAWN);

    var furtherInfo = new ArrayList<CaseEventSubject>();
    if (isFurtherInfoResponseOutstanding(scapDetail.getScap().getScapId())) {
      furtherInfo.add(FURTHER_INFO_RESPONSE);
    } else {
      furtherInfo.add(FURTHER_INFO_REQUESTED);
    }
    regulatorMap.put(QA, qa);
    regulatorMap.put(DECISIONS, descisions);
    regulatorMap.put(CONSULTATIONS, consultations);
    regulatorMap.put(FURTHER_INFO, furtherInfo);
    return regulatorMap;
  }
}
