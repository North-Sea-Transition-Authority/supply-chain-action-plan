package uk.co.nstauthority.scap.application.projectdetails;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.List;
import java.util.function.Function;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ContextConfiguration;
import uk.co.nstauthority.scap.AbstractControllerTest;
import uk.co.nstauthority.scap.energyportal.FieldQueryService;
import uk.co.nstauthority.scap.energyportal.FieldSelectable;
import uk.co.nstauthority.scap.fds.searchselector.RestSearchItem;
import uk.co.nstauthority.scap.fds.searchselector.RestSearchResult;
import uk.co.nstauthority.scap.fds.searchselector.SearchSelectorService;
import uk.co.nstauthority.scap.mvc.ReverseRouter;

@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes = ProjectDetailsRestController.class)
@WithMockUser
class ProjectDetailsRestControllerTest extends AbstractControllerTest {

  @MockBean
  FieldQueryService fieldQueryService;

  @MockBean
  SearchSelectorService searchSelectorService;

  @Test
  void getFieldSearchResults_assertCorrectResponse() throws Exception {
    var fieldSelectable1 = new FieldSelectable();
    fieldSelectable1.setFieldId("1");
    fieldSelectable1.setFieldName("test field 1");
    var fieldSelectable2 = new FieldSelectable();
    fieldSelectable2.setFieldId("2");
    fieldSelectable2.setFieldName("test field 2");

    var searchTerm = "test";

    var searchResult = new RestSearchResult(List.of(
        new RestSearchItem(fieldSelectable1.getFieldId(), fieldSelectable1.getSelectionText()),
        new RestSearchItem(fieldSelectable2.getFieldId(), fieldSelectable2.getSelectionText())
    ));

    when(searchSelectorService.search(eq(searchTerm), any(Function.class)))
        .thenReturn(searchResult);

    var response = mockMvc.perform(
        get(
            ReverseRouter.route(on(ProjectDetailsRestController.class).getFieldSearchResults(null)))
            .param("term", searchTerm))
        .andExpect(status().isOk())
        .andReturn()
        .getResponse();

    assertThat(response.getContentType()).isEqualTo("application/json");
    assertThat(response.getContentAsString())
        .isEqualTo("{\"results\":[{\"id\":\"1\",\"text\":\"test field 1\"},{\"id\":\"2\",\"text\":\"test field 2\"}]}");
  }
}
