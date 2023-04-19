package uk.co.nstauthority.scap.scap.detail;

import static uk.co.nstauthority.scap.scap.detail.ScapDetailStatus.APPROVED;
import static uk.co.nstauthority.scap.scap.detail.ScapDetailStatus.DRAFT;
import static uk.co.nstauthority.scap.scap.detail.ScapDetailStatus.SUBMITTED;

import java.time.Clock;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import javax.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.authentication.ServiceUserDetail;
import uk.co.nstauthority.scap.authentication.UserDetailService;
import uk.co.nstauthority.scap.error.exception.ScapBadRequestException;
import uk.co.nstauthority.scap.error.exception.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.permissionmanagement.TeamId;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberService;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;
import uk.co.nstauthority.scap.scap.copy.CopyService;
import uk.co.nstauthority.scap.scap.organisationgroup.OrganisationGroupForm;
import uk.co.nstauthority.scap.scap.scap.Scap;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.submit.ReviewAndSubmitForm;

@Service
public class ScapDetailService {

  private final ScapDetailRepository scapDetailRepository;

  private final UserDetailService userDetailService;

  private final TeamService teamService;

  private final TeamMemberService teamMemberService;
  private final Clock clock;

  private final List<CopyService> copyServices;

  private static final Set<ScapDetailStatus> canBeWithdrawn = Set.of(SUBMITTED, APPROVED);

  @Autowired
  public ScapDetailService(ScapDetailRepository scapDetailRepository,
                           UserDetailService userDetailService,
                           List<CopyService> copyServices,
                           TeamService teamService,
                           TeamMemberService teamMemberService,
                           Clock clock) {
    this.scapDetailRepository = scapDetailRepository;
    this.userDetailService = userDetailService;
    this.teamService = teamService;
    this.teamMemberService = teamMemberService;
    this.clock = clock;
    this.copyServices = copyServices;
  }

  @Transactional
  public ScapDetail createDraftScapDetail(Scap scap) {
    return createDraftScapDetail(scap, NewScapType.DRAFT_UPDATE);
  }

  public ScapDetail createDraftScapDetail(Scap scap, NewScapType isReinstatement) {
    var latestScapDetail = findLatestByScapIdAndStatusIn(scap.getScapId(), ScapDetailStatus.getReinstateableStatuses());

    var versionNumber = latestScapDetail.map(ScapDetail::getVersionNumber).orElse(0) + 1;

    var isLatestScapDetail = true;
    var userId = userDetailService
        .getUserDetail()
        .wuaId()
        .intValue();

    var newScapDetail = new ScapDetail(scap,
        versionNumber,
        isLatestScapDetail,
        ScapDetailStatus.DRAFT,
        clock.instant(),
        userId);

    if (latestScapDetail.isPresent()) {
      var oldScapDetail = latestScapDetail.get();
      oldScapDetail.setTipFlag(false);
      scapDetailRepository.save(oldScapDetail);
      updateDraftInfo(oldScapDetail, newScapDetail, isReinstatement);
    } else {
      scapDetailRepository.save(newScapDetail);
    }
    return newScapDetail;
  }

  private void updateDraftInfo(ScapDetail oldScapDetail, ScapDetail newScapDetail, NewScapType isReinstatement) {
    newScapDetail.setTierOneContractor(oldScapDetail.isTierOneContractor());
    newScapDetail.setParentScap(oldScapDetail.getParentScap());
    scapDetailRepository.save(newScapDetail);

    copyServices
        .stream()
        .sorted(Comparator.comparing(CopyService::runOrder))
        .forEach(copyService -> copyService.copyEntity(oldScapDetail, newScapDetail, isReinstatement));
  }

  public ScapDetail getById(Integer scapDetailId) {
    return scapDetailRepository.findById(scapDetailId).orElseThrow(
        () -> new ScapEntityNotFoundException(
            String.format("Could not find a ScapDetail with ID [%d]", scapDetailId)
        ));
  }

  public ScapDetail getLatestByScap(Scap scap) {
    return findLatestByScap(scap).orElseThrow(
        () -> new ScapEntityNotFoundException(
            String.format("Could not find a ScapDetail for Scap with ID [%d]", scap.getId())
        ));
  }

  public Optional<ScapDetail> findLatestByScap(Scap scap) {
    return scapDetailRepository.findAllByScap(scap)
        .stream()
        .filter(ScapDetail::getTipFlag)
        .findFirst();
  }

  public Optional<ScapDetail> findLatestByScapId(ScapId scapId) {
    return scapDetailRepository.findFirstByScapIdAndTipFlag(scapId.scapId(), true);
  }

  public Optional<ScapDetail> findLatestByScapIdAndStatus(ScapId scapId, ScapDetailStatus status) {
    return scapDetailRepository.findFirstByScapIdAndStatusOrderByVersionNumberDesc(scapId.scapId(), status);
  }

  public Optional<ScapDetail> findLatestByScapIdAndStatusIn(ScapId scapId, List<ScapDetailStatus> statuses) {
    return scapDetailRepository.findFirstByScapIdAndStatusInOrderByVersionNumberDesc(scapId.scapId(), statuses);
  }

  public Optional<ScapDetail> findLatestByScapIdAndStatusNotIn(ScapId scapId, List<ScapDetailStatus> statuses) {
    return scapDetailRepository.findFirstByScapIdAndStatusNotInOrderByVersionNumberDesc(scapId.scapId(), statuses);
  }

  public ScapDetail getLatestByScapIdAndStatusNotIn(ScapId scapId, List<ScapDetailStatus> statuses) {
    return findLatestByScapIdAndStatusNotIn(scapId, statuses)
        .orElseThrow(
            () -> new ScapEntityNotFoundException(
                String.format("Could not find a ScapDetail with ID [%d]", scapId.scapId())
            ));
  }

  public Optional<ScapDetail> findLatestSubmitted(ScapId scapId) {
    return scapDetailRepository.findFirstByScapIdAndTipFlagAndStatus(scapId.scapId(), true, SUBMITTED);
  }

  public ScapDetail getLatestByScapIdAndStatus(ScapId scapId, ScapDetailStatus status) {
    return findLatestByScapIdAndStatus(scapId, status).orElseThrow(
        () -> new ScapEntityNotFoundException(
            String.format("Could not find a ScapDetail for Scap with ID: [%d] in status: %s",
                scapId.scapId(),
                status.getEnumName())
        ));
  }

  public ScapDetail getLatestByScapIdOrThrow(ScapId scapId) {
    return findLatestByScapId(scapId).orElseThrow(
        () -> new ScapEntityNotFoundException(
            String.format("Could not find a ScapDetail for Scap with ID [%d]", scapId.scapId())
        ));
  }

  public ScapDetail getLatestSubmittedScapDetail(ScapId scapId) {
    return findLatestSubmitted(scapId).orElseThrow(
        () -> new ScapEntityNotFoundException(
            String.format("Could not find a ScapDetail for Scap with ID [%s]", scapId.scapId())
        ));
  }

  public Optional<ScapDetail> findByScapIdAndVersionNumber(ScapId scapId, Integer versionNumber) {
    return scapDetailRepository.findByScapIdAndVersionNumber(scapId.scapId(), versionNumber);
  }

  public ScapDetail getByScapIdAndVersionNumber(ScapId scapId, Integer versionNumber) {
    return findByScapIdAndVersionNumber(scapId, versionNumber).orElseThrow(
        () -> new ScapEntityNotFoundException(
            String.format("Could not find a ScapDetail for Scap with ID [%s]", scapId.scapId())
        ));
  }

  public List<ScapDetail> findAllByScap(Scap scap) {
    return scapDetailRepository.findAllByScap(scap);
  }

  public boolean isUpdateInProgress(ScapId scapId) {
    var optionalDraftUpdate = findLatestByScapIdAndStatus(scapId, DRAFT);
    return optionalDraftUpdate.map(scapDetail -> scapDetail.getVersionNumber() > 1).orElse(false);
  }

  public List<ScapDetail> getAllVersionsForUser(Scap scap) {
    var applicableVersions = findAllByScap(scap);
    var filterDraft = teamService.userIsMemberOfRegulatorTeam(userDetailService.getUserDetail());
    if (filterDraft) {
      return applicableVersions.stream()
          .filter(detail -> detail.getStatus() != ScapDetailStatus.DELETED)
          .filter(detail -> detail.getStatus() != ScapDetailStatus.DRAFT)
          .toList();
    }
    return applicableVersions.stream()
        .filter(detail -> detail.getStatus() != ScapDetailStatus.DELETED)
        .toList();
  }

  @Transactional
  public void submitScap(ScapDetail scapDetail, ReviewAndSubmitForm form) {
    scapDetail.setStatus(SUBMITTED);
    scapDetail.setSubmittedTimestamp(clock.instant());
    scapDetail.setApprovedByStakeholders(form.getApprovedByStakeholders());
    scapDetailRepository.save(scapDetail);
  }

  @Transactional
  public void approveScap(ScapDetail scapDetail) {
    if (!scapDetail.getStatus().equals(SUBMITTED)) {
      throw new ScapBadRequestException("Cannot approve SCAP reference: %s that has not been submitted"
          .formatted(scapDetail.getScap().getReference()));
    }

    scapDetail.setStatus(ScapDetailStatus.APPROVED);
    scapDetail.setApprovedTimestamp(clock.instant());
    scapDetailRepository.save(scapDetail);
  }

  @Transactional
  public void closeOutScap(ScapDetail scapDetail) {
    if (!scapDetail.getStatus().equals(SUBMITTED)) {
      throw new ScapBadRequestException("Cannot approve SCAP reference: %s that has not been submitted"
          .formatted(scapDetail.getScap().getReference()));
    }
    scapDetail.setStatus(ScapDetailStatus.CLOSED_OUT);
    scapDetail.setApprovedTimestamp(clock.instant());
    scapDetailRepository.save(scapDetail);
  }

  @Transactional
  public void withdrawScap(ScapDetail scapDetail) {
    if (!canBeWithdrawn.contains(scapDetail.getStatus())) {
      throw new ScapBadRequestException("Cannot withdraw SCAP reference: %s that has not been submitted"
          .formatted(scapDetail.getScap().getReference()));
    }
    var updatedScaps = new ArrayList<ScapDetail>();
    var draftScaps = scapDetailRepository.findAllByScapIdAndStatus(scapDetail.getScap().getId(),
        ScapDetailStatus.DRAFT);
    for (var detail : draftScaps) {
      detail.setStatus(ScapDetailStatus.DELETED);
      detail.setTipFlag(false);
      updatedScaps.add(detail);
    }
    scapDetail.setStatus(ScapDetailStatus.WITHDRAWN);
    scapDetail.setApprovedTimestamp(clock.instant());
    updatedScaps.add(scapDetail);
    scapDetailRepository.saveAll(updatedScaps);
  }

  @Transactional
  public void reinstateScap(Scap scap) {
    var draftDetail = createDraftScapDetail(scap, NewScapType.REINSTATEMENT);
    var reviewForm = new ReviewAndSubmitForm();
    reviewForm.setApprovedByStakeholders(true);
    submitScap(draftDetail, reviewForm);
  }

  @Transactional
  public void deleteScapById(ScapId scapId) {
    var scapDetail = getLatestByScapIdAndStatus(scapId, DRAFT);
    deleteScapDetail(scapDetail);
  }

  @Transactional
  public void deleteScapDetail(ScapDetail scapDetail) {
    if (scapDetail.getVersionNumber() != 1) {
      var previousVersion = scapDetail.getVersionNumber() - 1;
      findByScapIdAndVersionNumber(scapDetail.getScap().getScapId(), previousVersion)
          .ifPresent(previousScapDetail -> {
            previousScapDetail.setTipFlag(true);
            scapDetailRepository.save(previousScapDetail);
          });
    }

    scapDetail.setTipFlag(false);
    scapDetail.setStatus(ScapDetailStatus.DELETED);

    scapDetailRepository.save(scapDetail);
  }

  /**
   * Get the latest applicable scap that a specified user can work on.
   * This is latest Scap for an Industry User
   * and latest non DRAFT scap for a regulator user.
   * @param scapId the ID of the SCAP to look for.
   * @param user the user to check against.
   * @return the scap detail applicable to that user.
   */
  public ScapDetail getActionableScapDetail(ScapId scapId, ServiceUserDetail user) {
    var isRegulator = teamMemberService.isMemberOfTeam(
        new TeamId(teamService.getRegulatorTeam().getUuid()),
        user);
    if (isRegulator) {
      return getLatestByScapIdAndStatusNotIn(scapId, Collections.singletonList(DRAFT));
    } else {
      return getLatestByScapIdOrThrow(scapId);
    }
  }

  public void setTierOneContractor(ScapDetail scapDetail,
                                   Scap parentScap,
                                   OrganisationGroupForm form) {
    scapDetail.setTierOneContractor(form.getIsTierOneContractor());
    scapDetail.setParentScap(parentScap);
    scapDetailRepository.save(scapDetail);
  }
}
