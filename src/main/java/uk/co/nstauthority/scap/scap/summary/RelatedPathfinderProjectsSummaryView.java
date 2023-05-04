package uk.co.nstauthority.scap.scap.summary;

import java.util.List;

public record RelatedPathfinderProjectsSummaryView(Boolean hasPathfinderProjects,
                                                   List<String> pathfinderProjectNames,
                                                   String noPathfinderProjectsRationale) {

  static RelatedPathfinderProjectsSummaryView empty() {
    return new RelatedPathfinderProjectsSummaryView(null, null, null);
  }

  static RelatedPathfinderProjectsSummaryView noRelatedProjects(String noPathfinderProjectsRationale) {
    return new RelatedPathfinderProjectsSummaryView(false, null, noPathfinderProjectsRationale);
  }

  static RelatedPathfinderProjectsSummaryView relatedProjects(List<String> pathfinderProjectNames) {
    return new RelatedPathfinderProjectsSummaryView(true, pathfinderProjectNames, null);
  }
}
