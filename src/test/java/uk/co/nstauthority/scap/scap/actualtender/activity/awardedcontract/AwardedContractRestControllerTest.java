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
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.boot.test.mock.mockito.SpyBean;
import org.springframework.http.MediaType;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ContextConfiguration;
import uk.co.fivium.energyportalapi.generated.types.Country;
import uk.co.nstauthority.scap.AbstractControllerTest;
import uk.co.nstauthority.scap.energyportal.CountryService;
import uk.co.nstauthority.scap.fds.searchselector.SearchSelectorService;
import uk.co.nstauthority.scap.mvc.ReverseRouter;

@ExtendWith(MockitoExtension.class)
@WithMockUser
@ContextConfiguration(classes = AwardedContractRestController.class)
class AwardedContractRestControllerTest extends AbstractControllerTest {

  @SpyBean
  SearchSelectorService searchSelectorService;

  @MockBean
  CountryService countryService;

  @Test
  void getCountrySearchResults() throws Exception {
    var searchTerm = "United Kingdom";
    var countries = List.of(
        new Country(0, "United Kingdom", null, null),
        new Country(68, "Continental Shelf United Kingdom Sector", null, null)
    );

    when(countryService.searchCountries(searchTerm, AwardedContractRestController.SEARCH_PURPOSE))
        .thenReturn(countries);

    mockMvc.perform(get(
        ReverseRouter.route(on(AwardedContractRestController.class).getCountrySearchResults(searchTerm))))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(content().json("""
      {"results":[{"id":"0","text":"United Kingdom"},{"id":"68","text":"Continental Shelf United Kingdom Sector"}]}
"""));
  }
}
