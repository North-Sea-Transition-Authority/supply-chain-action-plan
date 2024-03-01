package uk.co.nstauthority.scap.scap.projectdetails;

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
import org.springframework.http.MediaType;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ContextConfiguration;
import uk.co.fivium.energyportalapi.generated.types.Facility;
import uk.co.fivium.energyportalapi.generated.types.Field;
import uk.co.nstauthority.scap.AbstractControllerTest;
import uk.co.nstauthority.scap.energyportal.FacilityService;
import uk.co.nstauthority.scap.energyportal.FieldService;
import uk.co.nstauthority.scap.fds.searchselector.RestSearchItem;
import uk.co.nstauthority.scap.fds.searchselector.RestSearchResult;
import uk.co.nstauthority.scap.mvc.ReverseRouter;

@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes = ProjectDetailsRestController.class)
@WithMockUser
class ProjectDetailsRestControllerTest extends AbstractControllerTest {

  @MockBean
  FieldService fieldService;

  @MockBean
  FacilityService facilityService;

  @Test
  void getFieldSearchResults_assertCorrectResponse() throws Exception {
    var searchTerm = "test";
    var fields = List.of(
        Field.newBuilder()
            .fieldId(1)
            .fieldName("test field 1")
            .build(),
        Field.newBuilder()
            .fieldId(2)
            .fieldName("test field 2")
            .build()
    );
    var fieldsSearchResult = new RestSearchResult(List.of(
        new RestSearchItem(fields.get(0).getFieldId().toString(), fields.get(0).getFieldName()),
        new RestSearchItem(fields.get(1).getFieldId().toString(), fields.get(1).getFieldName())
    ));

    when(fieldService.getFieldsByName(searchTerm, ProjectDetailsRestController.FIELD_SEARCH_REQUEST_PURPOSE))
        .thenReturn(fields);
    when(fieldService.getFieldsSearchResult(fields)).thenReturn(fieldsSearchResult);


    mockMvc.perform(get(
        ReverseRouter.route(on(ProjectDetailsRestController.class).getFieldSearchResults(null)))
            .param("term", searchTerm))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(content().json("""
{"results":[{"id":"1","text":"test field 1"},{"id":"2","text":"test field 2"}]}
"""));
    }

  @Test
  void getFacilitySearchResults() throws Exception {
    var searchTerm = "test";
    var facility = Facility.newBuilder()
        .id(1)
        .name("test facility")
        .type(null)
        .status(null)
        .isInUkcs(null)
        .build();
    var facilities = List.of(facility);
    var facilitiesRestSearchResult = new RestSearchResult(List.of(
        new RestSearchItem(String.valueOf(facility.getId()), facility.getName())
    ));

    when(facilityService.searchFacilities(searchTerm, ProjectDetailsRestController.FACILITIES_SEARCH_REQUEST_PURPOSE))
        .thenReturn(facilities);
    when(facilityService.facilitiesToRestSearchResult(facilities)).thenReturn(facilitiesRestSearchResult);

    var response = mockMvc.perform(get(
        ReverseRouter.route(on(ProjectDetailsRestController.class).getFacilitySearchResults(null)))
        .param("term", searchTerm))
        .andExpect(status().isOk())
        .andExpect(content().contentType(MediaType.APPLICATION_JSON))
        .andExpect(content().json("""
{"results":[{"id":"1","text":"test facility"}]}
"""))
        .andReturn()
        .getResponse();
  }
}
