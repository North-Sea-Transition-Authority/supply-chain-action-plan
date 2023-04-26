package uk.co.nstauthority.scap.scap.casemanagement;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import uk.co.nstauthority.scap.authentication.UserDetailService;
import uk.co.nstauthority.scap.energyportal.EnergyPortalUserDto;
import uk.co.nstauthority.scap.energyportal.EnergyPortalUserService;
import uk.co.nstauthority.scap.energyportal.WebUserAccountId;
import uk.co.nstauthority.scap.file.FileUploadService;
import uk.co.nstauthority.scap.file.UploadedFile;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.summary.files.FileUploadSummaryView;
import uk.co.nstauthority.scap.util.DateUtil;
import uk.co.nstauthority.scap.workarea.updaterequests.UpdateRequest;
import uk.co.nstauthority.scap.workarea.updaterequests.UpdateRequestService;
import uk.co.nstauthority.scap.workarea.updaterequests.UpdateRequestType;

@Service
public class CaseEventService {

  private final UserDetailService userDetailService;

  private final EnergyPortalUserService energyPortalUserService;

  private final CaseEventRepository caseEventRepository;

  private final TeamService teamService;

  private final ScapDetailService scapDetailService;

  private final UpdateRequestService updateRequestService;

  private final FileUploadService fileUploadService;

  private static final Set<CaseEventSubject> RESPONSE_ACTIONS = Set.of(
      CaseEventSubject.FURTHER_INFO_RESPONSE,
      CaseEventSubject.SCAP_SUBMITTED,
      CaseEventSubject.QA_COMMENT
  );

  private static final Set<ScapDetailStatus> TERMINAL_DETAIL_STATUS = Set.of(
      ScapDetailStatus.CLOSED_OUT,
      ScapDetailStatus.WITHDRAWN,
      ScapDetailStatus.DELETED);

  @Autowired
  public CaseEventService(UserDetailService userDetailService,
                          EnergyPortalUserService energyPortalUserService,
                          CaseEventRepository caseEventRepository,
                          TeamService teamService,
                          ScapDetailService scapDetailService,
                          UpdateRequestService updateRequestService,
                          FileUploadService fileUploadService) {
    this.userDetailService = userDetailService;
    this.energyPortalUserService = energyPortalUserService;
    this.caseEventRepository = caseEventRepository;
    this.teamService = teamService;
    this.scapDetailService = scapDetailService;
    this.updateRequestService = updateRequestService;
    this.fileUploadService = fileUploadService;
  }

  @Transactional
  public CaseEvent recordNewEvent(CaseEventSubject subject,
                                  ScapDetail scapDetail,
                                  Integer scapVersion,
                                  String comments) {
    return recordNewEvent(subject, scapDetail, scapVersion, comments, null);
  }

  public CaseEvent recordNewEvent(CaseEventSubject subject,
                                  ScapDetail scapDetail,
                                  Integer scapVersion,
                                  String comments,
                                  UUID fileId) {
    updateRequestService.resolveUpdateRequest(scapDetail, subject);
    UploadedFile uploadedFile = null;
    if (fileId != null) {
      uploadedFile = fileUploadService.getUploadedFile(fileId);
    }


    var caseEvent = new CaseEvent();
    caseEvent.setCaseEventSubject(subject);
    caseEvent.setEventTime(Instant.now());
    caseEvent.setScapId(scapDetail.getScap().getScapId().scapId());
    caseEvent.setVersionNumber(scapVersion);
    caseEvent.setComments(comments);
    caseEvent.setUploadedFile(uploadedFile);

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

    var updateRequests = updateRequestService.findAllByScapId(scapId);

    var userDisplayNames = energyPortalUserService.findByWuaIds(users)
        .stream()
        .collect(Collectors.toMap(EnergyPortalUserDto::webUserAccountId, EnergyPortalUserDto::displayName));

    return caseEvents
        .stream()
        .sorted(Comparator.comparing(CaseEvent::getEventTime).reversed())
        .map(caseEvent ->
            new CaseEventView(
                caseEvent
                    .getTimelineEventSubject()
                    .getDisplayName(),
                caseEvent.getScapId(),
                caseEvent.getVersionNumber(),
                DateUtil.instantToString(caseEvent.getEventTime()),
                userDisplayNames.get(caseEvent.getEventByWuaId()),
                caseEvent.getComments(),
                updateRequests
                    .stream()
                    .filter(updateRequest -> updateRequest.getCaseEvent().getId().equals(caseEvent.getId()))
                    .findFirst()
                    .map(UpdateRequest::getDueDate)
                    .map(DateUtil::localDateToString)
                    .orElse(null),
                getDateOfResponseToInfoRequest(caseEvent)
                    .map(DateUtil::instantToString)
                    .orElse(null),
                getFileUploadView(caseEvent.getScapId(), caseEvent.getUploadedFile())))
        .toList();
  }

  public Optional<Instant> getDateOfResponseToInfoRequest(CaseEvent infoRequest) {
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
    var scapDetail = scapDetailService.getActionableScapDetail(scapId, user);

    var actionMap = new LinkedHashMap<String, List<CaseEventSubject>>();
    if (teamService.userIsMemberOfRegulatorTeam(user)) {
      actionMap.putAll(getRegulatorTeamActions(scapDetail));
    }
    return actionMap;
  }

  private Map<String, List<CaseEventSubject>> getRegulatorTeamActions(ScapDetail scapDetail) {

    var regulatorMap = new HashMap<String, List<CaseEventSubject>>();
    var qa = new ArrayList<CaseEventSubject>();
    var consultations = new ArrayList<CaseEventSubject>();
    var descisions = new ArrayList<CaseEventSubject>();
    var furtherInfo = new ArrayList<CaseEventSubject>();

    if (ScapDetailStatus.SUBMITTED.equals(scapDetail.getStatus())) {
      qa.add(CaseEventSubject.QA_COMMENT);
      consultations.add(CaseEventSubject.SCAP_CONSULTATION_REQUESTED);
      consultations.add(CaseEventSubject.SCAP_CONSULTATION_RESPONSE);
      descisions.add(CaseEventSubject.SCAP_APPROVED);
      descisions.add(CaseEventSubject.SCAP_WITHDRAWN);
      if (updateRequestService.getUpdateDueDate(scapDetail.getScap().getScapId(),
          UpdateRequestType.FURTHER_INFORMATION).isPresent()) {
        furtherInfo.add(CaseEventSubject.FURTHER_INFO_RESPONSE);
      } else {
        furtherInfo.add(CaseEventSubject.FURTHER_INFO_REQUESTED);
      }
    } else if (ScapDetailStatus.APPROVED.equals(scapDetail.getStatus())) {
      furtherInfo.add(CaseEventSubject.SCAP_UPDATE_REQUESTED);
      descisions.add(CaseEventSubject.SCAP_APPROVED);
    } else if (TERMINAL_DETAIL_STATUS.contains(scapDetail.getStatus())) {
      descisions.add(CaseEventSubject.SCAP_REINSTATED);
    }
    regulatorMap.put(CaseEventGroups.QA.getDisplayName(), qa);
    regulatorMap.put(CaseEventGroups.DECISIONS.getDisplayName(), descisions);
    regulatorMap.put(CaseEventGroups.CONSULTATIONS.getDisplayName(), consultations);
    regulatorMap.put(CaseEventGroups.FURTHER_INFO.getDisplayName(), furtherInfo);
    return regulatorMap;
  }

  private FileUploadSummaryView getFileUploadView(Integer scapId, UploadedFile uploadedFile) {
    if (uploadedFile != null) {
      return new FileUploadSummaryView(
          uploadedFile.getFilename(),
          uploadedFile.getDescription(),
          ReverseRouter.route(on(CaseEventsDocumentController.class)
              .download(new ScapId(scapId), uploadedFile.getId())));
    }
    return null;
  }
}
