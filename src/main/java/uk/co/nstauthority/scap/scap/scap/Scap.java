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

  public Scap() {

  }

  public Scap(Integer organisationGroupId, Instant createdTimestamp) {
    this.organisationGroupId = organisationGroupId;
    this.createdTimestamp = createdTimestamp;
  }

  @VisibleForTesting
  public Scap(Integer id) {
    this.id = id;
  }

  public Integer getId() {
    return id;
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
}
