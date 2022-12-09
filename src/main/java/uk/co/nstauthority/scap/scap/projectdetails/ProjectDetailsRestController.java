package uk.co.nstauthority.scap.scap.projectdetails;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import uk.co.nstauthority.scap.energyportal.FacilityService;
import uk.co.nstauthority.scap.energyportal.FieldService;
import uk.co.nstauthority.scap.fds.searchselector.RestSearchResult;

@RestController
@RequestMapping("/data")
class ProjectDetailsRestController {

  static final String FIELD_SEARCH_REQUEST_PURPOSE = "Search fields for SCAP project details";
  static final String FACILITIES_SEARCH_REQUEST_PURPOSE = "Get facilities for SCAP project details";

  private final FieldService fieldService;
  private final FacilityService facilityService;

  @Autowired
  ProjectDetailsRestController(FieldService fieldService, FacilityService facilityService) {
    this.fieldService = fieldService;
    this.facilityService = facilityService;
  }

  @GetMapping("/field")
  public RestSearchResult getFieldSearchResults(@RequestParam(value = "term", required = false) String term) {
    var fields = fieldService.getFieldsByName(term, FIELD_SEARCH_REQUEST_PURPOSE);
    return fieldService.getFieldsSearchResult(fields);
  }

  @GetMapping("/facility")
  public RestSearchResult getFacilitySearchResults(@RequestParam(value = "term", required = false) String term) {
    var facilities = facilityService.searchFacilities(term, FACILITIES_SEARCH_REQUEST_PURPOSE);
    return facilityService.facilitiesToRestSearchResult(facilities);
  }
}
