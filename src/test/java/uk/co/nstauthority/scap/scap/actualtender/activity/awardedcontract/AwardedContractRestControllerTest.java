package uk.co.nstauthority.scap.scap.actualtender.activity.awardedcontract;

import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.MediaType;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.bean.override.mockito.MockitoBean;
import uk.co.fivium.energyportalapi.generated.types.CountryV2;
import uk.co.nstauthority.scap.AbstractControllerTest;
import uk.co.nstauthority.scap.energyportal.CountryService;
import uk.co.nstauthority.scap.fds.searchselector.RestSearchItem;
import uk.co.nstauthority.scap.fds.searchselector.RestSearchResult;
import uk.co.nstauthority.scap.mvc.ReverseRouter;

@ExtendWith(MockitoExtension.class)
@WithMockUser
@ContextConfiguration(classes = AwardedContractRestController.class)
class AwardedContractRestControllerTest extends AbstractControllerTest {

  @MockitoBean
  CountryService countryService;

  @Test
  void getCountrySearchResults() throws Exception {
    var searchTerm = "United Kingdom";
    var countries = List.of(
        new CountryV2("United Kingdom", "GB"),
        new CountryV2("Continental Shelf United Kingdom Sector", "UST")
    );
    var countriesSearchResult = new RestSearchResult(List.of(
        new RestSearchItem(countries.get(0).getIsoCode(), countries.get(0).getName()),
        new RestSearchItem(countries.get(1).getIsoCode(), countries.get(1).getName())
    ));

    when(countryService.searchCountries(searchTerm, AwardedContractRestController.SEARCH_PURPOSE))
        .thenReturn(countries);
    when(countryService.getCountrySearchResults(countries)).thenReturn(countriesSearchResult);

    mockMvc.perform(get(
        ReverseRouter.route(on(AwardedContractRestController.class).getCountrySearchResults(searchTerm))))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(content().json("""
      {"results":[{"id":"GB","text":"United Kingdom"},{"id":"UST","text":"Continental Shelf United Kingdom Sector"}]}
"""));
  }
}
