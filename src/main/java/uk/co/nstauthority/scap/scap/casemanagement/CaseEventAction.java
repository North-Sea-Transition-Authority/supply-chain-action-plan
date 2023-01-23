package uk.co.nstauthority.scap.scap.casemanagement;

import uk.co.nstauthority.scap.error.exception.IllegalUtilClassInstantiationException;

public class CaseEventAction {

  private CaseEventAction() {
    throw new IllegalUtilClassInstantiationException(CaseEventAction.class);
  }

  public static final String QA = "QA";

  public static final String SUBMIT = "SUBMIT";

  public static final String INFO_REQUESTED = "INFO_REQUESTED";

  public static final String CONSULTATION_REQUESTED = "CONSULTATION_REQUESTED";
}
