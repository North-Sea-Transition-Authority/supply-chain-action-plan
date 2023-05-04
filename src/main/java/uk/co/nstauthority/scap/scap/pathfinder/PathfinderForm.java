package uk.co.nstauthority.scap.scap.pathfinder;

import java.util.List;
import java.util.Set;
import uk.co.fivium.formlibrary.input.StringInput;

public class PathfinderForm {

  public static final String HAS_PATHFINDER_PROJECTS_FIELD = "hasPathfinderProjects";
  public static final String PATHFINDER_PROJECTS_SELECTOR_FIELD = "pathfinderProjectSelector";
  public static final String NO_PATHFINDER_PROJECT_RATIONALE_FIELD = "noPathfinderProjectRationale";

  private Boolean hasPathfinderProjects;
  private List<Integer> pathfinderProjectIds;
  private Object pathfinderProjectSelector;
  private final StringInput noPathfinderProjectRationale;

  public PathfinderForm() {
    this.noPathfinderProjectRationale = new StringInput(
        NO_PATHFINDER_PROJECT_RATIONALE_FIELD,
        "rationale for not publishing this information on Pathfinder"
    );
  }

  public static PathfinderForm from(PathfinderProjectsOverview pathfinderProjectsOverview,
                                    Set<PathfinderProject> relatedPathfinderProjects) {
    var form = new PathfinderForm();
    form.setHasPathfinderProjects(pathfinderProjectsOverview.getHasRelatedPathfinderProjects());
    form.setNoPathfinderProjectRationale(pathfinderProjectsOverview.getNoPathfinderProjectsRationale());
    var projectIds = relatedPathfinderProjects.stream()
        .map(PathfinderProject::getPathfinderProjectId)
        .toList();
    form.setPathfinderProjectIds(projectIds);
    return form;
  }

  public List<Integer> getPathfinderProjectIds() {
    return pathfinderProjectIds;
  }

  public void setPathfinderProjectIds(List<Integer> pathfinderProjectIds) {
    this.pathfinderProjectIds = pathfinderProjectIds;
  }

  public Boolean getHasPathfinderProjects() {
    return hasPathfinderProjects;
  }

  public void setHasPathfinderProjects(Boolean hasPathfinderProjects) {
    this.hasPathfinderProjects = hasPathfinderProjects;
  }

  public StringInput getNoPathfinderProjectRationale() {
    return noPathfinderProjectRationale;
  }

  public void setNoPathfinderProjectRationale(String noPathfinderProjectRationale) {
    this.noPathfinderProjectRationale.setInputValue(noPathfinderProjectRationale);
  }

  public Object getPathfinderProjectSelector() {
    return pathfinderProjectSelector;
  }

  public void setPathfinderProjectSelector(Object pathfinderProjectSelector) {
    this.pathfinderProjectSelector = pathfinderProjectSelector;
  }
}
