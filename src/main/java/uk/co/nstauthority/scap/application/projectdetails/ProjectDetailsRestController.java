package uk.co.nstauthority.scap.application.projectdetails;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import uk.co.nstauthority.scap.energyportal.FieldQueryService;
import uk.co.nstauthority.scap.fds.searchselector.RestSearchResult;
import uk.co.nstauthority.scap.fds.searchselector.SearchSelectorService;

@RestController
@RequestMapping("/data/field")
class ProjectDetailsRestController {

  private final FieldQueryService fieldQueryService;
  private final SearchSelectorService searchSelectorService;

  @Autowired
  ProjectDetailsRestController(FieldQueryService fieldQueryService, SearchSelectorService searchSelectorService) {
    this.fieldQueryService = fieldQueryService;
    this.searchSelectorService = searchSelectorService;
  }

  @GetMapping
  public RestSearchResult getFieldSearchResults(@RequestParam(value = "term", required = false) String term) {
    return searchSelectorService.search(term, fieldQueryService::getFieldSearchableResults);
  }
}
