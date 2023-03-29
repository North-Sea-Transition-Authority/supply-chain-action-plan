package uk.co.nstauthority.scap.scap.detail;

import java.time.Instant;
import uk.co.nstauthority.scap.scap.scap.Scap;

public class ScapDetailEntityTestUtil {

  public static Builder scapDetailBuilder() {
    return new Builder();
  }

  public static class Builder {
    private Integer scapDetailId;
    private Scap scap;
    private Integer versionNumber;
    private Boolean tipFlag;
    private ScapDetailStatus scapDetailStatus;
    private Instant createdTimestamp;
    private Integer createdByUserId;
    private Instant submittedTimestamp;
    private Instant approvedTimestamp;
    private Boolean tierOneContractor;
    private Scap parentScap;

    public Builder withScapDetailId(Integer scapDetailId) {
      this.scapDetailId = scapDetailId;
      return this;
    }

    public Builder withScap(Scap scap) {
      this.scap = scap;
      return this;
    }

    public Builder withVersionNumber(Integer versionNumber) {
      this.versionNumber = versionNumber;
      return this;
    }

    public Builder withTipFlag(Boolean tipFlag) {
      this.tipFlag = tipFlag;
      return this;
    }

    public Builder withStatus(ScapDetailStatus scapDetailStatus) {
      this.scapDetailStatus = scapDetailStatus;
      return this;
    }

    public Builder withCreatedTimestamp(Instant createdTimestamp) {
      this.createdTimestamp = createdTimestamp;
      return this;
    }

    public Builder withCreatedByUserId(Integer createdByUserId) {
      this.createdByUserId = createdByUserId;
      return this;
    }

    public Builder withSubmittedTimestamp(Instant submittedTimestamp) {
      this.submittedTimestamp = submittedTimestamp;
      return this;
    }

    public Builder withApprovedTimestamp(Instant approvedTimestamp) {
      this.approvedTimestamp = approvedTimestamp;
      return this;
    }

    public Builder withTierOneContractor(Boolean tierOneContractor) {
      this.tierOneContractor = tierOneContractor;
      return this;
    }

    public Builder withParentScap(Scap parentScap) {
      this.parentScap = parentScap;
      return this;
    }

    public ScapDetail build() {
      var scapDetail = new ScapDetail(scapDetailId);
      scapDetail.setScap(scap);
      scapDetail.setVersionNumber(versionNumber);
      scapDetail.setTipFlag(tipFlag);
      scapDetail.setStatus(scapDetailStatus);
      scapDetail.setCreatedTimestamp(createdTimestamp);
      scapDetail.setCreatedByUserId(createdByUserId);
      scapDetail.setSubmittedTimestamp(submittedTimestamp);
      scapDetail.setApprovedTimestamp(approvedTimestamp);
      scapDetail.setTierOneContractor(tierOneContractor);
      scapDetail.setParentScap(parentScap);
      return scapDetail;
    }
  }
}
