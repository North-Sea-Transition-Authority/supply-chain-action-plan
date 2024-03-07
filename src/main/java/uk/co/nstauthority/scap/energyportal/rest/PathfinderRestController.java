package uk.co.nstauthority.scap.energyportal.rest;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import uk.co.nstauthority.scap.endpointvalidation.annotations.HasAnyPermissionForScap;
import uk.co.nstauthority.scap.energyportal.PathfinderProjectService;
import uk.co.nstauthority.scap.fds.searchselector.RestSearchResult;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.scap.ScapService;

@RestController
@RequestMapping("/data/pathfinder/{scapId}")
@HasAnyPermissionForScap(permissions = RolePermission.VIEW_SCAP)
public class PathfinderRestController {

  static final String SEARCH_PURPOSE = "Search Pathfinder projects for SCAP Pathfinder Projects section";
  private final PathfinderProjectService pathfinderProjectService;
  private final ScapService scapService;

  @Autowired
  PathfinderRestController(PathfinderProjectService pathfinderProjectService,
                           ScapService scapService) {
    this.pathfinderProjectService = pathfinderProjectService;
    this.scapService = scapService;
  }

  @GetMapping
  public RestSearchResult getPathfinderSearchResults(@PathVariable("scapId") ScapId scapId,
                                                     @RequestParam(value = "term", required = false) String term) {
    var scap = scapService.getScapById(scapId);

    return pathfinderProjectService.searchProjects(term, SEARCH_PURPOSE, scap.getOrganisationGroupId());
  }
}
