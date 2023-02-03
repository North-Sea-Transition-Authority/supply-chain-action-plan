package uk.co.nstauthority.scap.scap.detail;

import java.time.Clock;
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
    var isLatestScapDetail = true;
    var userId = userDetailService
        .getUserDetail()
        .wuaId()
        .intValue();

    var scapDetail = new ScapDetail(scap, 1, isLatestScapDetail, ScapDetailStatus.DRAFT, clock.instant(), userId);
    scapDetailRepository.save(scapDetail);
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

  public ScapDetail getLatestScapDetailByScapIdOrThrow(ScapId scapId) {
    return getLatestScapDetailByScapId(scapId).orElseThrow(
        () -> new ScapEntityNotFoundException(
            String.format("Could not find a ScapDetail for Scap with ID [%d]", scapId.scapId())
        ));
  }

  public Optional<ScapDetail> getByScapIdAndVersionNumber(ScapId scapId, Integer versionNumber) {
    return scapDetailRepository.findByScapIdAndVersionNumber(scapId.scapId(), versionNumber);
  }

  @Transactional
  public void submitScap(ScapDetail scapDetail, ReviewAndSubmitForm form) {
    scapDetail.setStatus(ScapDetailStatus.SUBMITTED);
    scapDetail.setSubmittedTimestamp(clock.instant());
    scapDetail.setApprovedByStakeholders(form.getApprovedByStakeholders());
    scapDetailRepository.save(scapDetail);
  }

  @Transactional
  public void approveScap(ScapDetail scapDetail) {
    if (!scapDetail.getStatus().equals(ScapDetailStatus.SUBMITTED)) {
      throw new ScapBadRequestException("Cannot approve a SCAP that has not been submitted");
    }

    scapDetail.setApprovedTimestamp(clock.instant());
    scapDetail.setStatus(ScapDetailStatus.APPROVED);
    scapDetailRepository.save(scapDetail);
  }

  @Transactional
  public void deleteScapById(ScapId scapId) {
    var scapDetail = getLatestScapDetailByScapIdOrThrow(scapId);
    if (scapDetail.getVersionNumber() != 1) {
      var previousVersion = scapDetail.getVersionNumber() - 1;
      getByScapIdAndVersionNumber(scapId, previousVersion)
          .ifPresent(previousScapDetail -> {
            previousScapDetail.setTipFlag(true);
            scapDetailRepository.save(previousScapDetail);
          });
    }

    scapDetail.setVersionNumber(-1);
    scapDetail.setTipFlag(false);
    scapDetail.setStatus(ScapDetailStatus.DELETED);

    scapDetailRepository.save(scapDetail);
  }
}
