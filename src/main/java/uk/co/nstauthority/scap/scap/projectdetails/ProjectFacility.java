package uk.co.nstauthority.scap.scap.projectdetails;

import com.google.common.annotations.VisibleForTesting;
import java.time.Instant;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

@Entity
@Table(name = "project_facilities")
class ProjectFacility {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Integer id;

  @ManyToOne
  @JoinColumn(name = "project_details_id")
  private ProjectDetails projectDetails;

  private Integer facilityId;

  private Instant createdTimestamp;

  public ProjectFacility() {
  }

  @VisibleForTesting
  ProjectFacility(Integer id) {
    this.id = id;
  }

  ProjectFacility(ProjectDetails projectDetails, Instant createdTimestamp, Integer facilityId) {
    this.projectDetails = projectDetails;
    this.createdTimestamp = createdTimestamp;
    this.facilityId = facilityId;
  }

  Integer getId() {
    return id;
  }

  ProjectDetails getProjectDetails() {
    return projectDetails;
  }

  Integer getFacilityId() {
    return facilityId;
  }

  @VisibleForTesting
  void setFacilityId(Integer facilityId) {
    this.facilityId = facilityId;
  }

  Instant getCreatedTimestamp() {
    return createdTimestamp;
  }
}
