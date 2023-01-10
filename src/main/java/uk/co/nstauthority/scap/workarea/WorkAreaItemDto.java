package uk.co.nstauthority.scap.workarea;

import java.sql.Timestamp;
import java.time.Instant;
import java.util.Objects;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.scap.ScapId;

public record WorkAreaItemDto(Integer scapId,
                              Integer scapVersionNumber,
                              String reference,
                              String projectName,
                              Integer organisationGroupId,
                              ScapDetailStatus status,
                              Boolean projectClosedOut,
                              Boolean hasContractingPerformance,
                              Boolean hasActualTender,
                              Boolean hasPlannedTender,
                              Instant createdTimestamp,
                              Instant submittedTimestamp) {

  public WorkAreaItemDto(Integer scapId, Integer scapVersionNumber, String reference, String projectName,
                         Integer organisationGroupId, String status, Boolean projectClosedOut,
                         Boolean hasContractingPerformance, Boolean hasActualTender, Boolean hasPlannedTender,
                         Timestamp createdTimestamp, Timestamp submittedTimestamp) {
    this(scapId, scapVersionNumber, reference, projectName, organisationGroupId, ScapDetailStatus.valueOf(status),
        projectClosedOut, hasContractingPerformance, hasActualTender, hasPlannedTender,
        timestampToInstant(createdTimestamp), timestampToInstant(submittedTimestamp));
  }

  private static Instant timestampToInstant(Timestamp timestamp) {
    if (Objects.isNull(timestamp)) {
      return null;
    }
    return timestamp.toInstant();
  }
}
