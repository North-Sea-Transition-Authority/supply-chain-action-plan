package uk.co.nstauthority.scap.scap.casemanagement;

import java.util.Objects;
import java.util.Optional;

public record CaseEventView(String caseEventSubject,
                            Integer scapId,
                            Integer versionNumber,
                            String formattedTime,
                            String userDisplayName,
                            String comments) {
  public boolean hasComments() {
    return (Objects.nonNull(comments) && !comments.isEmpty());
  }
}
