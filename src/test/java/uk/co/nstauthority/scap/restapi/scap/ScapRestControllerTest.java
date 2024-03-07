package uk.co.nstauthority.scap.restapi.scap;

import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ContextConfiguration;
import uk.co.nstauthority.scap.AbstractScapSubmitterControllerTest;
import uk.co.nstauthority.scap.fds.searchselector.RestSearchItem;
import uk.co.nstauthority.scap.fds.searchselector.RestSearchResult;
import uk.co.nstauthority.scap.mvc.ReverseRouter;

@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes = ScapRestController.class)
@WithMockUser
class ScapRestControllerTest extends AbstractScapSubmitterControllerTest {

  @MockBean
  private ScapRestService scapRestService;

  @Test
  void searchScaps() throws Exception {
    var searchTerm = "2022/";
    var result = new RestSearchResult(List.of(
        new RestSearchItem("1", "SCAP/2022/1 - CENTRICA"),
        new RestSearchItem("2", "SCAP/2022/2 - SHELL")
    ));
    var resultJson = new ObjectMapper().writeValueAsString(result);

    when(scapRestService.searchScaps(searchTerm)).thenReturn(result);

    mockMvc.perform(get(
        ReverseRouter.route(on(ScapRestController.class).searchScaps(searchTerm))))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(content().json(resultJson));
  }
}