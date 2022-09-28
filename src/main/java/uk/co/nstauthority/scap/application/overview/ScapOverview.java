package uk.co.nstauthority.scap.application.overview;

import java.time.Instant;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;
import org.springframework.data.annotation.CreatedDate;

@Entity
@Table(name = "scap_overviews")
public class ScapOverview {

  @Id
  @GeneratedValue
  @Column(name = "scap_id")
  private Long id;

  // TODO SCAP2022-119: Change this type to OrganisationGroup rather than Long
  private Long operatorId;

  @CreatedDate
  private Instant createdTimestamp;

  public ScapOverview() {

  }

  public ScapOverview(Long operatorId) {
    this.operatorId = operatorId;
  }

  public Long getId() {
    return id;
  }

  public Instant getCreatedTimestamp() {
    return createdTimestamp;
  }

  public Long getOperatorId() {
    return operatorId;
  }

  public void setOperatorId(Long operatorId) {
    this.operatorId = operatorId;
  }
}
