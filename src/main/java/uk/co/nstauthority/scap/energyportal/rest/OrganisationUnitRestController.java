package uk.co.nstauthority.scap.energyportal.rest;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import uk.co.nstauthority.scap.energyportal.OrganisationUnitService;
import uk.co.nstauthority.scap.fds.searchselector.ManualEntryUtil;
import uk.co.nstauthority.scap.fds.searchselector.RestSearchResult;

@RestController
public class OrganisationUnitRestController {

  private final OrganisationUnitService organisationUnitService;

  static final String REQUEST_PURPOSE = "Search org units for SCAP actual tender activity form";

  @Autowired
  OrganisationUnitRestController(OrganisationUnitService organisationUnitService) {
    this.organisationUnitService = organisationUnitService;
  }

  @GetMapping("/data/organisation-unit")
  public RestSearchResult searchOrganisationUnitsWithManualEntry(
      @RequestParam(value = "term", required = false) String searchTerm) {
    var organisationUnits = organisationUnitService.searchByName(searchTerm, REQUEST_PURPOSE);
    var epaResultList = organisationUnitService.organisationUnitListToRestSearchResult(organisationUnits);
    return ManualEntryUtil.addManualEntry(epaResultList, searchTerm);
  }
}
