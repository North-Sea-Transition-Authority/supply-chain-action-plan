package uk.co.nstauthority.scap.scap.casemanagement;

import java.util.Objects;
import uk.co.nstauthority.scap.scap.summary.files.FileUploadSummaryView;

public record CaseEventView(String caseEventSubject,
                            Integer scapId,
                            Integer versionNumber,
                            String formattedTime,
                            String userDisplayName,
                            String comments,
                            String decisionRationale,
                            String dateOfUpdateDeadline,
                            String dateOfResponse,
                            FileUploadSummaryView supportingDocument) {
  public boolean hasComments() {
    return (Objects.nonNull(comments) && !comments.isEmpty());
  }

  public boolean hasDecisionRationale() {
    return (Objects.nonNull(decisionRationale) && !decisionRationale.isEmpty());
  }

  public boolean hasBeenRespondedTo() {
    return (Objects.nonNull(dateOfResponse));
  }

  public boolean hasDueDate() {
    return Objects.nonNull(dateOfUpdateDeadline);
  }

  public boolean hasSupportingDocument() {
    return (Objects.nonNull(supportingDocument));
  }
}
