package uk.co.nstauthority.scap.scap.casemanagement;

import uk.co.nstauthority.scap.error.exception.IllegalUtilClassInstantiationException;

public class CaseEventAction {

  private CaseEventAction() {
    throw new IllegalUtilClassInstantiationException(CaseEventAction.class);
  }

  public static final String QA = "QA";

  public static final String SUBMIT = "SUBMIT";

  public static final String INFO_REQUESTED = "INFO_REQUESTED";

  public static final String INFO_RESPONSE = "INFO_RESPONSE";

  public static final String CONSULTATION_REQUESTED = "CONSULTATION_REQUESTED";

  public static final String CONSULTATION_RESPONSE = "CONSULTATION_RESPONSE";

  public static final String APPROVED = "APPROVED";

  public static final String UPDATE = "UPDATE";

  public static final String CLOSED_OUT = "CLOSED_OUT";

  public static final String WITHDRAWN = "WITHDRAWN";
}
