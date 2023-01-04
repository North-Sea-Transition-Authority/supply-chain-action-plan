package uk.co.nstauthority.scap.scap.projectdetails;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.csrf;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.model;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.view;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.time.Clock;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.validation.BeanPropertyBindingResult;
import org.springframework.validation.BindingResult;
import org.springframework.validation.FieldError;
import uk.co.fivium.energyportalapi.generated.types.Field;
import uk.co.nstauthority.scap.AbstractControllerTest;
import uk.co.nstauthority.scap.energyportal.FieldService;
import uk.co.nstauthority.scap.fds.ErrorItem;
import uk.co.nstauthority.scap.file.FileUploadTemplate;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.projectdetails.supportingdocuments.SupportingDocumentService;
import uk.co.nstauthority.scap.scap.projectdetails.supportingdocuments.SupportingDocumentType;
import uk.co.nstauthority.scap.scap.scap.ScapService;
import uk.co.nstauthority.scap.scap.tasklist.TaskListController;
import uk.co.nstauthority.scap.validation.ValidationErrorOrderingService;

@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes = ProjectDetailsController.class)
@WithMockUser
class ProjectDetailsControllerTest extends AbstractControllerTest {

  @Autowired
  Clock clock;

  @MockBean
  ProjectDetailsFormService projectDetailsFormService;

  @MockBean
  ValidationErrorOrderingService validationErrorOrderingService;

  @MockBean
  ScapDetailService scapDetailService;

  @MockBean
  ProjectDetailsService projectDetailsService;

  @MockBean
  FieldService fieldService;

  @MockBean
  SupportingDocumentService supportingDocumentService;

  private Integer scapId;
  private ScapDetail scapDetail;
  private FileUploadTemplate fileUploadTemplate;

  @BeforeEach
  void setup() {
    scapId = 19;
    scapDetail = new ScapDetail(223);
    fileUploadTemplate = new FileUploadTemplate("#", "#", "#", "1", ".txt");
  }

  @Test
  void renderProjectDetailsForm_noProjectDetails_assertCorrectResponse() throws Exception {
    when(scapDetailService.getLatestScapDetailByScapIdOrThrow(scapId)).thenReturn(scapDetail);
    when(projectDetailsService.getProjectDetails(scapDetail)).thenReturn(Optional.empty());
    when(supportingDocumentService.buildFileUploadTemplate(scapId, SupportingDocumentType.ADDITIONAL_DOCUMENT))
        .thenReturn(fileUploadTemplate);

    var model = mockMvc.perform(
        get(ReverseRouter.route(on(ProjectDetailsController.class)
            .renderProjectDetailsForm(scapId))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/projectDetails"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(scapId))))
        .andExpect(model().attribute("fieldSearchRestUrl",
            ReverseRouter.route(on(ProjectDetailsRestController.class)
                .getFieldSearchResults(null))))
        .andExpect(model().attribute("projectTypesMap", ProjectType.getCheckboxItems()))
        .andReturn().getModelAndView().getModel();

    assertThat(model.get("form")).isInstanceOf(ProjectDetailsForm.class);
  }

  @Test
  void renderProjectDetailsForm_existingProjectDetails_assertCorrectResponse() throws Exception {
    var field = new Field(22, "Test field", null, null, null, null);
    var projectDetails = new ProjectDetails(scapDetail, clock.instant());
    projectDetails.setFieldId(field.getFieldId());
    projectDetails.setFieldName(field.getFieldName());
    var form = new ProjectDetailsForm();
    form.setFieldId(String.valueOf(field.getFieldId()));
    var preselectedItem = Map.of(String.valueOf(field.getFieldId()), field.getFieldName());

    when(scapDetailService.getLatestScapDetailByScapIdOrThrow(scapId)).thenReturn(scapDetail);
    when(projectDetailsService.getProjectDetails(scapDetail)).thenReturn(Optional.of(projectDetails));
    when(projectDetailsFormService.getForm(projectDetails, Collections.emptyList())).thenReturn(form);
    when(projectDetailsFormService.getPreselectedField(field.getFieldId())).thenReturn(Optional.of(preselectedItem));
    when(supportingDocumentService.buildFileUploadTemplate(scapId, SupportingDocumentType.ADDITIONAL_DOCUMENT))
        .thenReturn(fileUploadTemplate);

    mockMvc.perform(
        get(ReverseRouter.route(on(ProjectDetailsController.class)
            .renderProjectDetailsForm(scapId))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/projectDetails"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(scapId))))
        .andExpect(model().attribute("fieldSearchRestUrl",
            ReverseRouter.route(on(ProjectDetailsRestController.class)
                .getFieldSearchResults(null))))
        .andExpect(model().attribute("projectTypesMap", ProjectType.getCheckboxItems()))
        .andExpect(model().attribute("form", form))
        .andExpect(model().attribute("preselectedField", preselectedItem));


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

    when(scapDetailService.getLatestScapDetailByScapIdOrThrow(scapId)).thenReturn(scapDetail);
    when(projectDetailsFormService.validate(eq(form), any(BindingResult.class)))
        .thenReturn(bindingResult);
    when(validationErrorOrderingService.getErrorItemsFromBindingResult(form, bindingResult))
        .thenReturn(errorItems);
    when(supportingDocumentService.buildFileUploadTemplate(scapId, SupportingDocumentType.ADDITIONAL_DOCUMENT))
        .thenReturn(fileUploadTemplate);

    mockMvc.perform(
        post(ReverseRouter.route(on(ProjectDetailsController.class)
            .renderProjectDetailsForm(scapId)))
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/projectDetails"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(scapId))))
        .andExpect(model().attribute("fieldSearchRestUrl",
            ReverseRouter.route(on(ProjectDetailsRestController.class)
                .getFieldSearchResults(null))))
        .andExpect(model().attribute("projectTypesMap", ProjectType.getCheckboxItems()))
        .andExpect(model().attribute("errorList", errorItems));

    verify(projectDetailsService, never()).saveProjectDetails(any(), any());
  }

  @Test
  void saveProjectDetailsForm_valid_verifySaves() throws Exception {
    var form = new ProjectDetailsForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");
    var expectedRedirectUrl = ReverseRouter.route(on(TaskListController.class).renderTaskList(scapId));

    when(scapDetailService.getLatestScapDetailByScapIdOrThrow(scapId)).thenReturn(scapDetail);
    when(projectDetailsFormService.validate(eq(form), any(BindingResult.class)))
        .thenReturn(bindingResult);

    mockMvc.perform(
        post(ReverseRouter.route(on(ProjectDetailsController.class)
            .renderProjectDetailsForm(scapId)))
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().is3xxRedirection())
        .andExpect(view().name(String.format("redirect:%s", expectedRedirectUrl)));

    verify(projectDetailsService).saveProjectDetails(scapDetail, form);
  }
}
