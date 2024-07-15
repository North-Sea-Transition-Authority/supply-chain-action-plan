package uk.co.nstauthority.scap.scap.projectdetails;

import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import java.time.Instant;
import java.util.UUID;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UuidGenerator;
import uk.co.nstauthority.scap.scap.copy.ProjectDetailsChild;

@Entity
@Table(name = "project_fields")
public class ProjectField implements ProjectDetailsChild {

  @Id
  @UuidGenerator
  private UUID id;

  @ManyToOne
  private ProjectDetails projectDetails;

  private Integer fieldId;

  @CreationTimestamp
  private Instant createdTimestamp;

  public ProjectField() {
  }

  public ProjectField(ProjectDetails projectDetails, Integer fieldId, Instant createdTimestamp) {
    this.id = UUID.randomUUID();
    this.projectDetails = projectDetails;
    this.fieldId = fieldId;
    this.createdTimestamp = createdTimestamp;
  }

  @Override
  public UUID getId() {
    return id;
  }

  @Override
  public ProjectDetails getProjectDetails() {
    return projectDetails;
  }

  public Integer getFieldId() {
    return fieldId;
  }

  Instant getCreatedTimestamp() {
    return createdTimestamp;
  }

  @Override
  public void setId(UUID id) {
    this.id = id;
  }

  @Override
  public void setProjectDetails(ProjectDetails projectDetails) {
    this.projectDetails = projectDetails;
  }
}
