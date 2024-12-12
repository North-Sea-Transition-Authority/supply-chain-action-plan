package uk.co.nstauthority.scap.scap.casemanagement;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class CaseEventViewTest {

  @Test
  void caseEventView_HasComments_NoComment() {
    var timelineEventView = new CaseEventView(CaseEventSubject.SCAP_SUBMITTED.getDisplayName(),
        1,
        1,
        "2020-03-02 00:00:00",
        "Testing Account",
        null,
        null,
        null,
        null,
        null);

    assertFalse(timelineEventView.hasComments());
  }

  @Test
  void caseEventView_HasComments_Comments() {
    var timelineEventView = new CaseEventView(CaseEventSubject.SCAP_SUBMITTED.getDisplayName(),
        1,
        1,
        "2020-03-02 00:00:00",
        "Testing Account",
        "this event view has comments",
        null,
        null,
        null,
        null);

    assertTrue(timelineEventView.hasComments());
  }

  @Test
  void caseEventView_HasBeenResponded_NoResponse() {
    var caseEventView = new CaseEventView(CaseEventSubject.FURTHER_INFO_REQUESTED.getDisplayName(),
        1,
        1,
        "2020-03-02 00:00:00",
        "Testing Account",
        "this event view has comments",
        null,
        null,
        null,
        null);

    assertFalse(caseEventView.hasBeenRespondedTo());
  }

  @Test
  void caseEventView_HasBeenResponded_HasResponse() {
    var caseEventView = new CaseEventView(CaseEventSubject.FURTHER_INFO_REQUESTED.getDisplayName(),
        1,
        1,
        "2020-03-02 00:00:00",
        "Testing Account",
        "this event view has comments",
        "decision rationale",
        "2020-05-02 00:00:00",
        "2020-05-02 00:00:00",
        null);

    assertTrue(caseEventView.hasBeenRespondedTo());
  }

  @Test
  void caseEventView_hasDecisionRationale_noRationale() {
    var timelineEventView = new CaseEventView(CaseEventSubject.SCAP_SUBMITTED.getDisplayName(),
        1,
        1,
        "2020-03-02 00:00:00",
        "Testing Account",
        null,
        null,
        null,
        null,
        null);

    assertThat(timelineEventView.hasDecisionRationale()).isFalse();
  }

  @Test
  void caseEventView_hasDecisionRationale_hasRationale() {
    var timelineEventView = new CaseEventView(CaseEventSubject.SCAP_SUBMITTED.getDisplayName(),
        1,
        1,
        "2020-03-02 00:00:00",
        "Testing Account",
        "this event view has comments",
        "decision rationale",
        null,
        null,
        null);

    assertThat(timelineEventView.hasDecisionRationale()).isTrue();
  }
}
