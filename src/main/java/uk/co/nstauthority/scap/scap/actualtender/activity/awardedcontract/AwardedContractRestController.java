package uk.co.nstauthority.scap.scap.actualtender.activity.awardedcontract;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import uk.co.nstauthority.scap.energyportal.CountryService;
import uk.co.nstauthority.scap.fds.searchselector.RestSearchResult;
import uk.co.nstauthority.scap.fds.searchselector.SearchSelectorService;

@RestController
@RequestMapping("data/actual-tender/activity/actual-contract-award")
class AwardedContractRestController {

  public static final String SEARCH_PURPOSE = "Get country search results for SCAP actual contract award";

  private final SearchSelectorService searchSelectorService;
  private final CountryService countryService;

  @Autowired
  AwardedContractRestController(SearchSelectorService searchSelectorService, CountryService countryService) {
    this.searchSelectorService = searchSelectorService;
    this.countryService = countryService;
  }

  @GetMapping
  RestSearchResult getCountrySearchResults(@RequestParam(value = "term", required = false) String term) {
    var countries = countryService.searchCountries(term, SEARCH_PURPOSE);

    return searchSelectorService.search(term, CountryService.getCountrySearchableResults(countries));
  }
}
