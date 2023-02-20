package uk.co.nstauthority.scap.energyportal.rest;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.tuple;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.Collections;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ContextConfiguration;
import uk.co.fivium.energyportalapi.generated.types.OrganisationUnit;
import uk.co.nstauthority.scap.AbstractControllerTest;
import uk.co.nstauthority.scap.energyportal.OrganisationUnitService;
import uk.co.nstauthority.scap.fds.searchselector.ManualEntryUtil;
import uk.co.nstauthority.scap.fds.searchselector.RestSearchItem;
import uk.co.nstauthority.scap.fds.searchselector.RestSearchResult;
import uk.co.nstauthority.scap.mvc.ReverseRouter;

@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes = OrganisationUnitRestController.class)
@WithMockUser
class OrganisationUnitRestControllerTest extends AbstractControllerTest {

  @MockBean
  OrganisationUnitService organisationUnitService;

  @Test
  void searchOrganisationUnitsWithManualEntry() throws Exception {
    var searchTerm = "company name";
    var orgUnit = OrganisationUnit.newBuilder()
        .organisationUnitId(1)
        .name("FULL COMPANY NAME")
        .build();

    when(organisationUnitService.searchByName(searchTerm, OrganisationUnitRestController.REQUEST_PURPOSE))
        .thenReturn(Collections.singletonList(orgUnit));
    when(organisationUnitService.organisationUnitListToRestSearchResult(Collections.singletonList(orgUnit)))
        .thenCallRealMethod();

    var json = mockMvc.perform(get(
        ReverseRouter.route(on(OrganisationUnitRestController.class).searchOrganisationUnitsWithManualEntry(null)))
        .param("term", searchTerm))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andReturn()
        .getResponse()
        .getContentAsString();

    var searchResult = new ObjectMapper().readValue(json, RestSearchResult.class);

    assertThat(searchResult.getResults()).extracting(
        RestSearchItem::id,
        RestSearchItem::text
    ).containsExactly(
        tuple(ManualEntryUtil.addFreeTextPrefix(searchTerm), searchTerm),
        tuple(orgUnit.getOrganisationUnitId().toString(), orgUnit.getName())
    );
  }

}