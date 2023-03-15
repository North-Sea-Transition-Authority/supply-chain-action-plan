package uk.co.nstauthority.scap.workarea.updaterequests;

import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventSubject.FURTHER_INFO_RESPONSE;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventSubject.SCAP_SUBMITTED;

import java.util.List;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventSubject;

public enum UpdateRequestType {
  FURTHER_INFORMATION(List.of(FURTHER_INFO_RESPONSE, SCAP_SUBMITTED)),
  UPDATE(List.of(SCAP_SUBMITTED));

  private final List<CaseEventSubject> resolvedBy;

  UpdateRequestType(List<CaseEventSubject> resolvedBy) {
    this.resolvedBy = resolvedBy;
  }

  public List<CaseEventSubject> getResolvedBy() {
    return resolvedBy;
  }
}
