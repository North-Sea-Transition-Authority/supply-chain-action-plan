package uk.co.nstauthority.scap.energyportal.rest;

import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.Collections;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.test.context.ContextConfiguration;
import org.testcontainers.shaded.com.fasterxml.jackson.databind.ObjectMapper;
import uk.co.nstauthority.scap.AbstractScapSubmitterControllerTest;
import uk.co.nstauthority.scap.energyportal.PathfinderProjectService;
import uk.co.nstauthority.scap.fds.searchselector.RestSearchItem;
import uk.co.nstauthority.scap.fds.searchselector.RestSearchResult;
import uk.co.nstauthority.scap.mvc.ReverseRouter;

@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes = PathfinderRestController.class)
class PathfinderRestControllerTest extends AbstractScapSubmitterControllerTest {

  private static final ObjectMapper MAPPER = new ObjectMapper();

  @MockBean
  private PathfinderProjectService pathfinderProjectService;

  @Test
  void getPathfinderSearchResults() throws Exception {
    var searchTerm = "my pathfinder project";
    var restResult = new RestSearchResult(
        Collections.singletonList(new RestSearchItem("99", "Pathfinder project name"))
    );
    var expectedJson = MAPPER.writeValueAsString(restResult);

    when(pathfinderProjectService.searchProjects(
        searchTerm,
        PathfinderRestController.SEARCH_PURPOSE,
        scap.getOrganisationGroupId()
    )).thenReturn(restResult);

    mockMvc.perform(get(
        ReverseRouter.route(on(PathfinderRestController.class).getPathfinderSearchResults(scap.getScapId(), searchTerm)))
            .with(authenticatedScapUser()))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(content().json(expectedJson));
  }
}
