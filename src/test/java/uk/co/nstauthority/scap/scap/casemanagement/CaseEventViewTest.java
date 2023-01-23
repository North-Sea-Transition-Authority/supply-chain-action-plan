package uk.co.nstauthority.scap.scap.casemanagement;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class CaseEventViewTest {

  @Test
  void timelineEventView_HasComments_NoComment() {
    var timelineEventView = new CaseEventView(CaseEventSubject.SCAP_SUBMITTED.getDisplayName(),
        1,
        1,
        "2020-03-02 00:00:00",
        "Testing Account",
        null);

    assertFalse(timelineEventView.hasComments());
  }

  @Test
  void timelineEventView_HasComments_Comments() {
    var timelineEventView = new CaseEventView(CaseEventSubject.SCAP_SUBMITTED.getDisplayName(),
        1,
        1,
        "2020-03-02 00:00:00",
        "Testing Account",
        "this event view has comments");

    assertTrue(timelineEventView.hasComments());
  }
}
