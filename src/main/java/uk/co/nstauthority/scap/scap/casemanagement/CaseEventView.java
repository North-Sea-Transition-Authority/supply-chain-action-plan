package uk.co.nstauthority.scap.scap.casemanagement;

import java.util.Objects;
import uk.co.nstauthority.scap.scap.summary.files.FileUploadSummaryView;

public record CaseEventView(String caseEventSubject,
                            Integer scapId,
                            Integer versionNumber,
                            String formattedTime,
                            String userDisplayName,
                            String comments,
                            String dateOfResponse,
                            FileUploadSummaryView supportingDocument) {
  public boolean hasComments() {
    return (Objects.nonNull(comments) && !comments.isEmpty());
  }

  public boolean hasBeenRespondedTo() {
    return (Objects.nonNull(dateOfResponse));
  }

  public boolean hasSupportingDocument() {
    return (Objects.nonNull(supportingDocument));
  }
}
