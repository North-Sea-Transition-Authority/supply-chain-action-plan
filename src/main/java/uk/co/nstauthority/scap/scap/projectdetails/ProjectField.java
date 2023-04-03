package uk.co.nstauthority.scap.scap.projectdetails;

import java.time.Instant;
import java.util.UUID;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import org.hibernate.annotations.CreationTimestamp;
import uk.co.nstauthority.scap.scap.copy.ProjectDetailsChild;

@Entity
@Table(name = "project_fields")
public class ProjectField implements ProjectDetailsChild {

  @Id
  @GeneratedValue(generator = "uuid")
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

  Integer getFieldId() {
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
