package uk.co.nstauthority.scap.application.overview;

import java.time.Instant;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "scap_overviews")
public class ScapOverview {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  @Column(name = "scap_id")
  private Integer id;

  private Integer organisationGroupId;

  private Instant createdTimestamp;

  public ScapOverview() {

  }

  public ScapOverview(Integer organisationGroupId) {
    this.organisationGroupId = organisationGroupId;
    this.createdTimestamp = Instant.now();
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
