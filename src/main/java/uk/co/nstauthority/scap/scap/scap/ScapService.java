package uk.co.nstauthority.scap.scap.scap;

import java.time.Clock;
import java.time.ZonedDateTime;
import javax.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.error.exception.ScapEntityNotFoundException;

@Service
public class ScapService {

  private final ScapRepository scapRepository;
  private final Clock clock;

  @Autowired
  public ScapService(ScapRepository scapRepository, Clock clock) {
    this.scapRepository = scapRepository;
    this.clock = clock;
  }

  @Transactional
  public Scap createScap(Integer organisationGroupId) {
    var reference = generateScapReference();
    var scap = new Scap(organisationGroupId, clock.instant(), reference);
    scapRepository.save(scap);
    return scap;
  }

  public Scap getScapById(Integer id) {
    return scapRepository.findById(id)
        .orElseThrow(() ->
            new ScapEntityNotFoundException(String.format("Could not find SCAP overview with id [%d]", id)));
  }

  @Transactional
  public void updateScapOrganisationGroup(Scap scap, Integer organisationGroupId) {
    scap.setOrganisationGroupId(organisationGroupId);
    scapRepository.save(scap);
  }

  private String generateScapReference() {
    var currentYear = ZonedDateTime.now(clock).getYear();
    var startOfYear = ZonedDateTime.of(
        currentYear, 1, 1,
        0, 0, 0, 0,
        clock.getZone());
    // Removing 1000 nanoseconds from instant, as highest precision of instants in postgres is 1 microsecond
    // so that if a SCAP is submitted 1 microsecond before midnight on 2022-12-31, then it will still be counted as being
    // in 2022, then the next possible timestamp is guaranteed to be in 2023
    var endOfYear = startOfYear.plusYears(1).minusNanos(1000);

    var scapsThisYear = scapRepository.countByCreatedTimestampBetween(startOfYear.toInstant(), endOfYear.toInstant());

    return "SCAP/%d/%d".formatted(currentYear, scapsThisYear + 1);
  }
}
