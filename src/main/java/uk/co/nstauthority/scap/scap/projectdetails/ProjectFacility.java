package uk.co.nstauthority.scap.scap.projectdetails;

import com.google.common.annotations.VisibleForTesting;
import java.time.Instant;
import java.util.UUID;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import org.hibernate.annotations.CreationTimestamp;
import uk.co.nstauthority.scap.scap.copy.ProjectDetailsChild;

@Entity
@Table(name = "project_facilities")
public class ProjectFacility implements ProjectDetailsChild {

  @Id
  @GeneratedValue(generator = "uuid")
  private UUID id;

  @ManyToOne
  @JoinColumn(name = "project_details_id")
  private ProjectDetails projectDetails;

  private Integer facilityId;

  @CreationTimestamp
  private Instant createdTimestamp;

  public ProjectFacility() {
  }

  @VisibleForTesting
  ProjectFacility(UUID id) {
    this.id = id;
  }

  public ProjectFacility(ProjectDetails projectDetails, Instant createdTimestamp, Integer facilityId) {
    this.projectDetails = projectDetails;
    this.createdTimestamp = createdTimestamp;
    this.facilityId = facilityId;
  }

  @Override
  public UUID getId() {
    return id;
  }

  @Override
  public ProjectDetails getProjectDetails() {
    return projectDetails;
  }

  @Override
  public void setId(UUID id) {
    this.id = id;
  }

  @Override
  public void setProjectDetails(ProjectDetails projectDetails) {
    this.projectDetails = projectDetails;
  }

  public Integer getFacilityId() {
    return facilityId;
  }

  @VisibleForTesting
  public void setFacilityId(Integer facilityId) {
    this.facilityId = facilityId;
  }

  Instant getCreatedTimestamp() {
    return createdTimestamp;
  }
}
