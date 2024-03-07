package uk.co.nstauthority.scap.scap.copy;

import java.util.UUID;
import uk.co.nstauthority.scap.scap.projectdetails.ProjectDetails;

public interface ProjectDetailsChild {

  void setId(UUID id);

  UUID getId();

  void setProjectDetails(ProjectDetails projectDetails);

  ProjectDetails getProjectDetails();
}
