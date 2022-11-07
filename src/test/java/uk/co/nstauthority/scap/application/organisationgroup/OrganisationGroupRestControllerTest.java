package uk.co.nstauthority.scap.application.organisationgroup;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.Collections;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.security.test.context.support.WithMockUser;
import uk.co.fivium.energyportalapi.generated.types.OrganisationGroup;
import uk.co.nstauthority.scap.AbstractControllerTest;
import uk.co.nstauthority.scap.fds.searchselector.RestSearchItem;
import uk.co.nstauthority.scap.fds.searchselector.RestSearchResult;
import uk.co.nstauthority.scap.mvc.ReverseRouter;

@ExtendWith(MockitoExtension.class)
@WebMvcTest(controllers = OrganisationGroupRestController.class)
@WithMockUser
class OrganisationGroupRestControllerTest extends AbstractControllerTest {

  @MockBean
  private OrganisationGroupService organisationGroupService;

  @Test
  void getOrganisationGroupSearchResults() throws Exception {
    var groupList = List.of(
        new OrganisationGroup(1, "Royal Dutch Shell", null, null, null, Collections.emptyList()),
        new OrganisationGroup(2, "Shell", null, null, null, Collections.emptyList())
    );

    when(organisationGroupService.getOrganisationGroupsByName(
        "shell", "Search organisation groups for SCAP form"))
        .thenReturn(groupList);
    when(organisationGroupService.organisationGroupsToSearchResult(groupList))
        .thenReturn(new RestSearchResult(List.of(
            new RestSearchItem("1", "Royal Dutch Shell"),
            new RestSearchItem("2", "Shell")
        )));

    var response = mockMvc.perform(
        get(
            ReverseRouter.route(on(OrganisationGroupRestController.class).getOrganisationGroupSearchResults(null)))
            .param("term", "shell"))
        .andExpect(status().isOk())
        .andReturn()
        .getResponse();

    assertThat(response.getContentType()).isEqualTo("application/json");
    assertThat(response.getContentAsString()).isEqualTo(
        "{\"results\":[{\"id\":\"1\",\"text\":\"Royal Dutch Shell\"},{\"id\":\"2\",\"text\":\"Shell\"}]}");
  }

}
