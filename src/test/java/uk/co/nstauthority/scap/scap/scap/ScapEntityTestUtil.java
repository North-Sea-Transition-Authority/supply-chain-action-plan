package uk.co.nstauthority.scap.scap.scap;

import java.time.Instant;

public class ScapEntityTestUtil {
  public static Builder scapBuilder() {
    return new Builder();
  }

  public static class Builder {
    private ScapId scapId = new ScapId(1);
    private Integer organisationGroupId = 55;
    private Instant createdTimestamp = Instant.now();
    private String reference = "SCAP/TEST/1";

    public Builder withScapId(ScapId scapId) {
      this.scapId = scapId;
      return this;
    }

    public Builder withOrganisationGroupId(Integer organisationGroupId) {
      this.organisationGroupId = organisationGroupId;
      return this;
    }

    public Builder withCreatedTimestamp(Instant createdTimestamp) {
      this.createdTimestamp = createdTimestamp;
      return this;
    }

    public Builder withReference(String reference) {
      this.reference = reference;
      return this;
    }

    public Scap build() {
      var scap = new Scap(scapId);
      scap.setOrganisationGroupId(organisationGroupId);
      scap.setReference(reference);
      scap.setCreatedTimestamp(createdTimestamp);
      return scap;
    }
  }
}
