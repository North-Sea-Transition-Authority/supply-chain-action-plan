package uk.co.nstauthority.scap.scap.projectdetails;

import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import java.time.Instant;
import java.util.UUID;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UuidGenerator;
import uk.co.nstauthority.scap.scap.copy.ProjectDetailsChild;

@Entity
@Table(name = "project_detail_types")
public class ProjectDetailType implements ProjectDetailsChild {

  @Id
  @UuidGenerator
  private UUID id;

  @ManyToOne
  @JoinColumn(name = "project_detail_id")
  private ProjectDetails projectDetails;

  @Enumerated(EnumType.STRING)
  private ProjectType projectType;

  @CreationTimestamp
  private Instant createdTimestamp;

  public ProjectDetailType() {
  }

  public ProjectDetailType(ProjectDetails projectDetails, Instant createdTimestamp) {
    this.projectDetails = projectDetails;
    this.createdTimestamp = createdTimestamp;
  }

  public ProjectDetailType(ProjectDetails projectDetails, Instant createdTimestamp, ProjectType projectType) {
    this.projectDetails = projectDetails;
    this.projectType = projectType;
    this.createdTimestamp = createdTimestamp;
  }

  @Override
  public UUID getId() {
    return id;
  }

  @Override
  public void setId(UUID id) {
    this.id = id;
  }

  @Override
  public ProjectDetails getProjectDetails() {
    return projectDetails;
  }

  @Override
  public void setProjectDetails(ProjectDetails projectDetails) {
    this.projectDetails = projectDetails;
  }

  public Instant getCreatedTimestamp() {
    return createdTimestamp;
  }

  public ProjectType getProjectType() {
    return projectType;
  }

  public void setProjectType(ProjectType projectType) {
    this.projectType = projectType;
  }
}
