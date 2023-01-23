package uk.co.nstauthority.scap.scap.casemanagement;

import java.time.Instant;
import java.util.Comparator;
import java.util.List;
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
                caseEvent.getComments()
            ))
        .toList();
  }
}
