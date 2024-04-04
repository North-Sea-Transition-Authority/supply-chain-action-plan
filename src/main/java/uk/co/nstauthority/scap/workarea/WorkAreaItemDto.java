package uk.co.nstauthority.scap.workarea;

import java.time.Instant;
import java.time.LocalDate;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.workarea.updaterequests.UpdateRequestType;

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
                              Instant submittedTimestamp,
                              LocalDate resolutionDate,
                              LocalDate dueDate,
                              UpdateRequestType updateRequestType) {
}
