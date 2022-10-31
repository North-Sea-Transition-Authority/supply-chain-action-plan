package uk.co.nstauthority.scap.application.projectdetails;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.csrf;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.model;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.view;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.validation.BeanPropertyBindingResult;
import org.springframework.validation.BindingResult;
import org.springframework.validation.FieldError;
import uk.co.nstauthority.scap.AbstractControllerTest;
import uk.co.nstauthority.scap.application.overview.ScapOverview;
import uk.co.nstauthority.scap.application.tasklist.TaskListController;
import uk.co.nstauthority.scap.fds.ErrorItem;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.validation.ValidationErrorOrderingService;

@ExtendWith(MockitoExtension.class)
@WebMvcTest(ProjectDetailsController.class)
@WithMockUser
class ProjectDetailsControllerTest extends AbstractControllerTest {

  @MockBean
  ProjectDetailsFormService projectDetailsFormService;

  @MockBean
  ValidationErrorOrderingService validationErrorOrderingService;

  private ScapOverview scap;

  @BeforeEach
  void setup() {
    scap = new ScapOverview(19);
  }

  @Test
  void renderProjectDetailsForm_assertCorrectResponse() throws Exception {
    mockMvc.perform(
        get(ReverseRouter.route(on(ProjectDetailsController.class)
            .renderProjectDetailsForm(scap.getId(), null))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/application/projectDetails"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(scap.getId()))))
        .andExpect(model().attribute("fieldSearchRestUrl",
            ReverseRouter.route(on(ProjectDetailsRestController.class)
                .getFieldSearchResults(null))))
        .andExpect(model().attribute("projectTypesMap", ProjectType.getCheckboxItems()));
  }

  @Test
  void saveProjectDetailsForm_invalid_verifyNeverSaves() throws Exception {
    var form = new ProjectDetailsForm();
    var errorField = "projectName";
    var errorMessage = "This field is required";
    var bindingResult = new BeanPropertyBindingResult(form, "form");
    bindingResult.addError(
        new FieldError("form", errorField, errorMessage)
    );
    var errorItems = List.of(
        new ErrorItem(10, errorField, errorMessage)
    );

    when(projectDetailsFormService.validate(eq(form), any(BindingResult.class)))
        .thenReturn(bindingResult);
    when(validationErrorOrderingService.getErrorItemsFromBindingResult(form, bindingResult))
        .thenReturn(errorItems);

    mockMvc.perform(
        post(ReverseRouter.route(on(ProjectDetailsController.class)
            .renderProjectDetailsForm(scap.getId(), null)))
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/application/projectDetails"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(scap.getId()))))
        .andExpect(model().attribute("fieldSearchRestUrl",
            ReverseRouter.route(on(ProjectDetailsRestController.class)
                .getFieldSearchResults(null))))
        .andExpect(model().attribute("projectTypesMap", ProjectType.getCheckboxItems()))
        .andExpect(model().attribute("errorList", errorItems));
  }

  @Test
  void saveProjectDetailsForm_valid_verifySaves() throws Exception {
    // TODO: Make the POST mapping actually save data, will be in second PR for this ticket
    var form = new ProjectDetailsForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");
    var expectedRedirectUrl = ReverseRouter.route(on(ProjectDetailsController.class).
        renderProjectDetailsForm(scap.getId(), null));

    when(projectDetailsFormService.validate(eq(form), any(BindingResult.class)))
        .thenReturn(bindingResult);

    mockMvc.perform(
        post(ReverseRouter.route(on(ProjectDetailsController.class)
            .renderProjectDetailsForm(scap.getId(), null)))
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().is3xxRedirection())
        .andExpect(view().name(String.format("redirect:%s", expectedRedirectUrl)));

    // TODO: verify that entity is saved
  }
}
