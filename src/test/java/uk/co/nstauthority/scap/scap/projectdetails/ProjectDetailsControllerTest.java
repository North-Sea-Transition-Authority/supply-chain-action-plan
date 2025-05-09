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
import java.time.Instant;
import java.util.Collections;
import java.util.List;
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
import uk.co.nstauthority.scap.AbstractScapSubmitterControllerTest;
import uk.co.nstauthority.scap.energyportal.FieldService;
import uk.co.nstauthority.scap.fds.ErrorItem;
import uk.co.nstauthority.scap.fds.addtolist.AddToListItem;
import uk.co.nstauthority.scap.file.FileUploadTemplate;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.projectdetails.supportingdocuments.SupportingDocumentService;
import uk.co.nstauthority.scap.scap.projectdetails.supportingdocuments.SupportingDocumentType;
import uk.co.nstauthority.scap.scap.tasklist.TaskListController;
import uk.co.nstauthority.scap.validation.ValidationErrorOrderingService;

@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes = ProjectDetailsController.class)
@WithMockUser
class ProjectDetailsControllerTest extends AbstractScapSubmitterControllerTest {

  @Autowired
  Clock clock;

  @MockBean
  ProjectDetailsFormService projectDetailsFormService;

  @MockBean
  ValidationErrorOrderingService validationErrorOrderingService;

  @MockBean
  ProjectDetailsService projectDetailsService;

  @MockBean
  FieldService fieldService;

  @MockBean
  SupportingDocumentService supportingDocumentService;

  private ScapDetail scapDetail;
  private FileUploadTemplate fileUploadTemplate;

  @BeforeEach
  void setup() {
    scapDetail = new ScapDetail(223);
    fileUploadTemplate = new FileUploadTemplate("#", "#", "#", "1", ".txt");
  }

  @Test
  void renderProjectDetailsForm_noProjectDetails_assertCorrectResponse() throws Exception {
    when(scapDetailService.getLatestByScapId(SCAP_ID)).thenReturn(scapDetail);
    when(projectDetailsService.findByScapDetail(scapDetail)).thenReturn(Optional.empty());
    when(supportingDocumentService.buildFileUploadTemplate(SCAP_ID, SupportingDocumentType.ADDITIONAL_DOCUMENT))
        .thenReturn(fileUploadTemplate);

    var modelAndView = mockMvc.perform(
        get(ReverseRouter.route(on(ProjectDetailsController.class)
            .renderProjectDetailsForm(SCAP_ID))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/projectDetails"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(SCAP_ID))))
        .andExpect(model().attribute("fieldSearchRestUrl",
            ReverseRouter.route(on(ProjectDetailsRestController.class)
                .getFieldSearchResults(null))))
        .andExpect(model().attribute("projectTypesMap", ProjectType.getCheckboxItems()))
        .andReturn()
        .getModelAndView();

    assertThat(modelAndView).isNotNull();
    assertThat(modelAndView.getModel().get("form")).isInstanceOf(ProjectDetailsForm.class);
  }

  @Test
  void renderProjectDetailsForm_existingProjectDetails_assertCorrectResponse() throws Exception {
    var projectDetails = new ProjectDetails(scapDetail, clock.instant());
    var fieldId = 22;
    var fieldIds = Collections.singleton(fieldId);
    var projectField = new ProjectField(projectDetails, fieldId, Instant.now());
    var projectFields = Collections.singletonList(projectField);
    var installationId = 33;
    var installationIds = Collections.singleton(installationId);
    var projectFacility = new ProjectFacility(projectDetails, Instant.now(), installationId);
    var projectFacilities = Collections.singletonList(projectFacility);
    var form = new ProjectDetailsForm();
    form.setFieldIds(Collections.singleton(fieldId));
    form.setInstallationIds(installationIds);
    var preselectedFields = List.of(
        new AddToListItem(String.valueOf(fieldId), "Test field", true)
    );
    var preselectedFacilities = Collections.singletonList(
        new AddToListItem(String.valueOf(installationId), "Test facility", true)
    );

    when(scapDetailService.getLatestByScapId(SCAP_ID)).thenReturn(scapDetail);
    when(projectDetailsService.findByScapDetail(scapDetail)).thenReturn(Optional.of(projectDetails));
    when(projectDetailsService.getProjectFields(projectDetails)).thenReturn(projectFields);
    when(projectDetailsService.getProjectFacilities(projectDetails)).thenReturn(projectFacilities);
    when(projectDetailsFormService.getForm(projectDetails, installationIds, fieldIds))
        .thenReturn(form);
    when(projectDetailsFormService.getPreselectedFields(fieldIds)).thenReturn(preselectedFields);
    when(projectDetailsFormService.getPreselectedFacilities(installationIds))
        .thenReturn(preselectedFacilities);
    when(supportingDocumentService.buildFileUploadTemplate(SCAP_ID, SupportingDocumentType.ADDITIONAL_DOCUMENT))
        .thenReturn(fileUploadTemplate);

    mockMvc.perform(
        get(ReverseRouter.route(on(ProjectDetailsController.class)
            .renderProjectDetailsForm(SCAP_ID))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/projectDetails"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(SCAP_ID))))
        .andExpect(model().attribute("fieldSearchRestUrl",
            ReverseRouter.route(on(ProjectDetailsRestController.class)
                .getFieldSearchResults(null))))
        .andExpect(model().attribute("projectTypesMap", ProjectType.getCheckboxItems()))
        .andExpect(model().attribute("form", form))
        .andExpect(model().attribute("preselectedFields", preselectedFields));

    verify(projectDetailsFormService).getPreselectedFields(form.getFieldIds());
    verify(projectDetailsFormService).getPreselectedFacilities(form.getInstallationIds());
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

    when(scapDetailService.getLatestByScapId(SCAP_ID)).thenReturn(scapDetail);
    when(projectDetailsFormService.validate(eq(form), any(BindingResult.class)))
        .thenReturn(bindingResult);
    when(validationErrorOrderingService.getErrorItemsFromBindingResult(form, bindingResult))
        .thenReturn(errorItems);
    when(supportingDocumentService.buildFileUploadTemplate(SCAP_ID, SupportingDocumentType.ADDITIONAL_DOCUMENT))
        .thenReturn(fileUploadTemplate);

    mockMvc.perform(
        post(ReverseRouter.route(on(ProjectDetailsController.class)
            .renderProjectDetailsForm(SCAP_ID)))
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/projectDetails"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(SCAP_ID))))
        .andExpect(model().attribute("fieldSearchRestUrl",
            ReverseRouter.route(on(ProjectDetailsRestController.class)
                .getFieldSearchResults(null))))
        .andExpect(model().attribute("projectTypesMap", ProjectType.getCheckboxItems()))
        .andExpect(model().attribute("errorList", errorItems));

    verify(projectDetailsFormService).getPreselectedFields(form.getFieldIds());
    verify(projectDetailsFormService).getPreselectedFacilities(form.getInstallationIds());
    verify(projectDetailsService, never()).saveProjectDetails(any(), any());
  }

  @Test
  void saveProjectDetailsForm_valid_verifySaves() throws Exception {
    var form = new ProjectDetailsForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");
    var expectedRedirectUrl = ReverseRouter.route(on(TaskListController.class).renderTaskList(SCAP_ID));

    when(scapDetailService.getLatestByScapId(SCAP_ID)).thenReturn(scapDetail);
    when(projectDetailsFormService.validate(eq(form), any(BindingResult.class)))
        .thenReturn(bindingResult);

    mockMvc.perform(
        post(ReverseRouter.route(on(ProjectDetailsController.class)
            .renderProjectDetailsForm(SCAP_ID)))
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().is3xxRedirection())
        .andExpect(view().name(String.format("redirect:%s", expectedRedirectUrl)));

    verify(projectDetailsService).saveProjectDetails(scapDetail, form);
  }
}
