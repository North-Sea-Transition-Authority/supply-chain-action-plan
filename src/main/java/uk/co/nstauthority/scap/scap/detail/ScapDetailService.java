package uk.co.nstauthority.scap.scap.detail;

import static uk.co.nstauthority.scap.scap.detail.ScapDetailStatus.DRAFT;
import static uk.co.nstauthority.scap.scap.detail.ScapDetailStatus.SUBMITTED;

import java.time.Clock;
import java.util.ArrayList;
import java.util.Optional;
import javax.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.authentication.UserDetailService;
import uk.co.nstauthority.scap.error.exception.ScapBadRequestException;
import uk.co.nstauthority.scap.error.exception.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.scap.scap.Scap;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.submit.ReviewAndSubmitForm;

@Service
public class ScapDetailService {

  private final ScapDetailRepository scapDetailRepository;

  private final UserDetailService userDetailService;
  private final Clock clock;

  @Autowired
  public ScapDetailService(ScapDetailRepository scapDetailRepository, UserDetailService userDetailService, Clock clock) {
    this.scapDetailRepository = scapDetailRepository;
    this.userDetailService = userDetailService;
    this.clock = clock;
  }

  @Transactional
  public void createDraftScapDetail(Scap scap) {
    var latestScapDetail = findLatestByScapIdAndStatus(scap.getScapId(), SUBMITTED);
    var versionNumber = latestScapDetail.map(ScapDetail::getVersionNumber).orElse(0) + 1;

    var isLatestScapDetail = true;
    var userId = userDetailService
        .getUserDetail()
        .wuaId()
        .intValue();

    if (latestScapDetail.isPresent()) {
      var detail = latestScapDetail.get();
      detail.setTipFlag(false);
      scapDetailRepository.save(detail);
    }
    var scapDetail = new ScapDetail(scap, versionNumber, isLatestScapDetail, ScapDetailStatus.DRAFT, clock.instant(), userId);
    scapDetailRepository.save(scapDetail);
  }

  public ScapDetail getById(Integer scapDetailId) {
    return scapDetailRepository.findById(scapDetailId).orElseThrow(
        () -> new ScapEntityNotFoundException(
            String.format("Could not find a ScapDetail with ID [%d]", scapDetailId)
        ));
  }

  public Optional<ScapDetail> getLatestScapDetailByScap(Scap scap) {
    return scapDetailRepository.findAllByScap(scap)
        .stream()
        .filter(ScapDetail::getTipFlag)
        .findFirst();
  }

  public ScapDetail getLatestScapDetailByScapOrThrow(Scap scap) {
    return getLatestScapDetailByScap(scap).orElseThrow(
        () -> new ScapEntityNotFoundException(
            String.format("Could not find a ScapDetail for Scap with ID [%d]", scap.getId())
        ));
  }

  public Optional<ScapDetail> getLatestScapDetailByScapId(ScapId scapId) {
    return scapDetailRepository.findFirstByScapIdAndTipFlag(scapId.scapId(), true);
  }

  public Optional<ScapDetail> findLatestByScapIdAndStatus(ScapId scapId, ScapDetailStatus status) {
    return scapDetailRepository.findFirstByScapIdAndStatusOrderByVersionNumberDesc(scapId.scapId(), status);
  }

  public ScapDetail getLatestByScapIdAndStatus(ScapId scapId, ScapDetailStatus status) {
    return findLatestByScapIdAndStatus(scapId, status).orElseThrow(
        () -> new ScapEntityNotFoundException(
            String.format("Could not find a ScapDetail for Scap with ID: [%d] in status: %s",
                scapId.scapId(),
                status.getEnumName())
        ));
  }

  public ScapDetail getLatestScapDetailByScapIdOrThrow(ScapId scapId) {
    return getLatestScapDetailByScapId(scapId).orElseThrow(
        () -> new ScapEntityNotFoundException(
            String.format("Could not find a ScapDetail for Scap with ID [%d]", scapId.scapId())
        ));
  }

  public Optional<ScapDetail> findLatestSubmittedScapDetail(ScapId scapId) {
    return scapDetailRepository.findFirstByScapIdAndTipFlagAndStatus(scapId.scapId(), true, SUBMITTED);
  }

  public ScapDetail getLatestSubmittedScapDetail(ScapId scapId) {
    return findLatestSubmittedScapDetail(scapId).orElseThrow(
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

  public boolean isUpdateInProgress(ScapId scapId) {
    var optionalDraftUpdate = findLatestByScapIdAndStatus(scapId, DRAFT);
    return optionalDraftUpdate.map(scapDetail -> scapDetail.getVersionNumber() > 1).orElse(false);
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
    if (!scapDetail.getStatus().equals(SUBMITTED)) {
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
}
