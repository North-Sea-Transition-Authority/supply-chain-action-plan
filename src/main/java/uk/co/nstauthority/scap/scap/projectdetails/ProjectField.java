package uk.co.nstauthority.scap.scap.projectdetails;

import java.time.Instant;
import java.util.UUID;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

@Entity
@Table(name = "project_fields")
class ProjectField {

  @Id
  private UUID id;

  @ManyToOne
  private ProjectDetails projectDetails;

  private Integer fieldId;

  private Instant createdTimestamp;

  public ProjectField() {
  }

  ProjectField(ProjectDetails projectDetails, Integer fieldId, Instant createdTimestamp) {
    this.id = UUID.randomUUID();
    this.projectDetails = projectDetails;
    this.fieldId = fieldId;
    this.createdTimestamp = createdTimestamp;
  }

  UUID getId() {
    return id;
  }

  ProjectDetails getProjectDetails() {
    return projectDetails;
  }

  Integer getFieldId() {
    return fieldId;
  }

  Instant getCreatedTimestamp() {
    return createdTimestamp;
  }
}
