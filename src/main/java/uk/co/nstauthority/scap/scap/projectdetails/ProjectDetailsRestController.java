package uk.co.nstauthority.scap.scap.projectdetails;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import uk.co.nstauthority.scap.energyportal.FacilityService;
import uk.co.nstauthority.scap.energyportal.FieldQueryService;
import uk.co.nstauthority.scap.fds.searchselector.RestSearchResult;
import uk.co.nstauthority.scap.fds.searchselector.SearchSelectorService;

@RestController
@RequestMapping("/data")
class ProjectDetailsRestController {

  static final String FACILITIES_SEARCH_REQUEST_PURPOSE = "Get facilities for SCAP project details";
  private final FieldQueryService fieldQueryService;
  private final SearchSelectorService searchSelectorService;
  private final FacilityService facilityService;

  @Autowired
  ProjectDetailsRestController(FieldQueryService fieldQueryService, SearchSelectorService searchSelectorService,
                               FacilityService facilityService) {
    this.fieldQueryService = fieldQueryService;
    this.searchSelectorService = searchSelectorService;
    this.facilityService = facilityService;
  }

  @GetMapping("/field")
  public RestSearchResult getFieldSearchResults(@RequestParam(value = "term", required = false) String term) {
    return searchSelectorService.search(term, fieldQueryService::getFieldSearchableResults);
  }

  @GetMapping("/facility")
  public RestSearchResult getFacilitySearchResults(@RequestParam(value = "term", required = false) String term) {
    var facilities = facilityService.searchFacilities(term, FACILITIES_SEARCH_REQUEST_PURPOSE);
    return facilityService.facilitiesToRestSearchResult(facilities);
  }
}
