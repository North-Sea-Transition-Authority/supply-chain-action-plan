package uk.co.nstauthority.scap.scap.projectdetails;

import java.time.Instant;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

@Entity
@Table(name = "project_detail_types")
class ProjectDetailType {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Integer id;

  @ManyToOne
  @JoinColumn(name = "project_detail_id")
  private ProjectDetails projectDetails;

  @Enumerated(EnumType.STRING)
  private ProjectType projectType;

  private Instant createdTimestamp;

  public ProjectDetailType() {
  }

  public ProjectDetailType(ProjectDetails projectDetails, Instant createdTimestamp) {
    this.projectDetails = projectDetails;
    this.createdTimestamp = createdTimestamp;
  }

  public ProjectDetails getProjectDetails() {
    return projectDetails;
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
