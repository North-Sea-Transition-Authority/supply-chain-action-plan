package uk.co.nstauthority.scap.scap.organisationgroup;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import uk.co.nstauthority.scap.fds.searchselector.RestSearchResult;

@RestController
public class OrganisationGroupRestController {

  private final OrganisationGroupService organisationGroupService;

  public OrganisationGroupRestController(OrganisationGroupService organisationGroupService) {
    this.organisationGroupService = organisationGroupService;
  }

  @GetMapping("/data/organisation-group")
  public RestSearchResult getOrganisationGroupSearchResults(@RequestParam(value = "term", required = false) String term) {
    var queryResults = organisationGroupService
        .getOrganisationGroupsByName(term, "Search organisation groups for SCAP form");

    return organisationGroupService.organisationGroupsToSearchResult(queryResults);
  }
}
