package uk.co.nstauthority.scap.scap.scap;

import com.google.common.annotations.VisibleForTesting;
import java.time.Instant;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "scaps")
public class Scap {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  @Column(name = "scap_id")
  private Integer id;

  private Integer organisationGroupId;

  private Instant createdTimestamp;

  private String reference;

  public Scap() {

  }

  public Scap(Integer organisationGroupId, Instant createdTimestamp, String reference) {
    this.organisationGroupId = organisationGroupId;
    this.createdTimestamp = createdTimestamp;
    this.reference = reference;
  }

  @VisibleForTesting
  public Scap(Integer id) {
    this.id = id;
  }

  public Integer getId() {
    return id;
  }

  public ScapId getScapId() {
    return new ScapId(id);
  }

  public Instant getCreatedTimestamp() {
    return createdTimestamp;
  }

  public Integer getOrganisationGroupId() {
    return organisationGroupId;
  }

  public void setOrganisationGroupId(Integer organisationGroupId) {
    this.organisationGroupId = organisationGroupId;
  }

  public String getReference() {
    return reference;
  }

  @VisibleForTesting
  public void setReference(String reference) {
    this.reference = reference;
  }
}
