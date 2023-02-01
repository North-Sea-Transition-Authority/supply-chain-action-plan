package uk.co.nstauthority.scap.scap.casemanagement;

import java.util.Objects;

public record CaseEventView(String caseEventSubject,
                            Integer scapId,
                            Integer versionNumber,
                            String formattedTime,
                            String userDisplayName,
                            String comments,
                            String dateOfResponse) {
  public boolean hasComments() {
    return (Objects.nonNull(comments) && !comments.isEmpty());
  }

  public boolean hasBeenRespondedTo() {
    return (Objects.nonNull(dateOfResponse));
  }
}
