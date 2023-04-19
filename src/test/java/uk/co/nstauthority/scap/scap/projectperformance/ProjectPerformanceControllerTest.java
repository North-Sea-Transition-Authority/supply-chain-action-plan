package uk.co.nstauthority.scap.scap.projectperformance;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.csrf;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.model;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.redirectedUrl;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.view;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.Optional;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.validation.BeanPropertyBindingResult;
import org.springframework.validation.BindingResult;
import org.springframework.validation.FieldError;
import uk.co.nstauthority.scap.AbstractScapSubmitterControllerTest;
import uk.co.nstauthority.scap.enumutil.YesNo;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.tasklist.TaskListController;

@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes = ProjectPerformanceController.class)
@WithMockUser
class ProjectPerformanceControllerTest extends AbstractScapSubmitterControllerTest {

  @MockBean
  ProjectPerformanceFormService projectPerformanceFormService;

  @MockBean
  ProjectPerformanceService projectPerformanceService;

  private ScapDetail scapDetail;

  @BeforeEach
  void setup() {
    scapDetail = new ScapDetail();
  }

  @Test
  void renderProjectPerformanceForm_NoExistingProjectPerformance() throws Exception {
    when(scapDetailService.getLatestByScapId(SCAP_ID)).thenReturn(scapDetail);
    when(projectPerformanceService.findByScapDetail(scapDetail)).thenReturn(Optional.empty());

    mockMvc.perform(get(
        ReverseRouter.route(on(ProjectPerformanceController.class).renderProjectPerformanceForm(SCAP_ID))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/projectPerformance"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(SCAP_ID))))
        .andExpect(model().attribute("radioItems", YesNo.getRadioOptions()))
        .andExpect(model().attributeExists("form"));
  }

  @Test
  void renderProjectPerformanceForm_HasExistingProjectPerformance() throws Exception {
    var projectPerformance = new ProjectPerformance();
    var form = new ProjectPerformanceForm();

    when(scapDetailService.getLatestByScapId(SCAP_ID)).thenReturn(scapDetail);
    when(projectPerformanceService.findByScapDetail(scapDetail))
        .thenReturn(Optional.of(projectPerformance));
    when(projectPerformanceFormService.getForm(projectPerformance)).thenReturn(form);

    mockMvc.perform(get(
        ReverseRouter.route(on(ProjectPerformanceController.class).renderProjectPerformanceForm(SCAP_ID))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/projectPerformance"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(SCAP_ID))))
        .andExpect(model().attribute("radioItems", YesNo.getRadioOptions()))
        .andExpect(model().attribute("form", form));
  }

  @Test
  void saveProjectPerformanceForm_InvalidForm_AssertErrors() throws Exception {
    var form = new ProjectPerformanceForm();
    var bindingResultWithErrors = new BeanPropertyBindingResult(form, "form");
    bindingResultWithErrors.addError(new FieldError(
        "form", "testField", "Test error message"
    ));

    when(scapDetailService.getLatestByScapId(SCAP_ID)).thenReturn(scapDetail);
    when(projectPerformanceService.findByScapDetail(scapDetail))
        .thenReturn(Optional.empty());
    when(projectPerformanceFormService.validate(eq(form), any(BindingResult.class)))
        .thenReturn(bindingResultWithErrors);

    mockMvc.perform(post(
        ReverseRouter.route(on(ProjectPerformanceController.class).renderProjectPerformanceForm(SCAP_ID)))
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/projectPerformance"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(SCAP_ID))))
        .andExpect(model().attribute("radioItems", YesNo.getRadioOptions()))
        .andExpect(model().attribute("form", form))
        .andExpect(model().attributeExists("errorList"));

    verify(projectPerformanceService, never()).createProjectPerformance(any(), any());
    verify(projectPerformanceService, never()).updateProjectPerformance(any(), any());
  }

  @Test
  void saveProjectPerformanceForm_NewProjectPerformance_AssertCreates() throws Exception {
    var form = new ProjectPerformanceForm();
    var bindingResultWithoutErrors = new BeanPropertyBindingResult(form, "form");
    var expectedRedirectUrl = ReverseRouter.route(on(TaskListController.class)
        .renderTaskList(SCAP_ID));

    when(scapDetailService.getLatestByScapId(SCAP_ID)).thenReturn(scapDetail);
    when(projectPerformanceService.findByScapDetail(scapDetail))
        .thenReturn(Optional.empty());
    when(projectPerformanceFormService.validate(eq(form), any(BindingResult.class)))
        .thenReturn(bindingResultWithoutErrors);

    mockMvc.perform(post(
        ReverseRouter.route(on(ProjectPerformanceController.class).renderProjectPerformanceForm(SCAP_ID)))
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().is3xxRedirection())
        .andExpect(redirectedUrl(expectedRedirectUrl));

    verify(projectPerformanceService).createProjectPerformance(scapDetail, form);
  }

  @Test
  void saveProjectPerformanceForm_ExistingProjectPerformance_AssertSaves() throws Exception {
    var form = new ProjectPerformanceForm();
    var bindingResultWithoutErrors = new BeanPropertyBindingResult(form, "form");
    var expectedRedirectUrl = ReverseRouter.route(on(TaskListController.class)
        .renderTaskList(SCAP_ID));
    var projectPerformance = new ProjectPerformance();

    when(scapDetailService.getLatestByScapId(SCAP_ID)).thenReturn(scapDetail);
    when(projectPerformanceService.findByScapDetail(scapDetail))
        .thenReturn(Optional.of(projectPerformance));
    when(projectPerformanceFormService.validate(eq(form), any(BindingResult.class)))
        .thenReturn(bindingResultWithoutErrors);

    mockMvc.perform(post(
        ReverseRouter.route(on(ProjectPerformanceController.class).renderProjectPerformanceForm(SCAP_ID)))
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().is3xxRedirection())
        .andExpect(redirectedUrl(expectedRedirectUrl));

    verify(projectPerformanceService).updateProjectPerformance(projectPerformance, form);
  }
}
