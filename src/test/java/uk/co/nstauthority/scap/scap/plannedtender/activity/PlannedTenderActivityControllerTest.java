package uk.co.nstauthority.scap.scap.plannedtender.activity;

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
import static uk.co.nstauthority.scap.mvc.ReverseRouter.emptyBindingResult;

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
import uk.co.nstauthority.scap.error.exception.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.RemunerationModel;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.plannedtender.PlannedTender;
import uk.co.nstauthority.scap.scap.plannedtender.PlannedTenderController;
import uk.co.nstauthority.scap.scap.plannedtender.PlannedTenderService;
import uk.co.nstauthority.scap.scap.plannedtender.hasplannedtender.HasPlannedTenderController;
import uk.co.nstauthority.scap.utils.EntityTestingUtil;
import uk.co.nstauthority.scap.validation.ValidationErrorOrderingService;

@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes = PlannedTenderActivityController.class)
@WithMockUser
class PlannedTenderActivityControllerTest extends AbstractScapSubmitterControllerTest {

  @MockBean
  PlannedTenderService plannedTenderService;

  @MockBean
  PlannedTenderActivityService plannedTenderActivityService;

  @MockBean
  PlannedTenderActivityFormService plannedTenderActivityFormService;

  @MockBean
  ValidationErrorOrderingService validationErrorOrderingService;
  private ScapDetail scapDetail;
  private PlannedTender plannedTender;


  @BeforeEach
  void setup() {
    scapDetail = new ScapDetail(scap, 1, ScapDetailStatus.DRAFT, EntityTestingUtil.dateToInstant(2000, 4, 23), 1);
    plannedTender = new PlannedTender(scapDetail, EntityTestingUtil.dateToInstant(2000, 4, 23));
  }

  @Test
  void renderPlannedTenderDetailForm() throws Exception {
    when(scapDetailService.getLatestByScap(scap)).thenReturn(scapDetail);
    when(plannedTenderService.getByScapDetail(scapDetail)).thenReturn(plannedTender);
    when(plannedTenderActivityService.hasExistingTenderDetails(plannedTender)).thenReturn(false);

    mockMvc.perform(
        get(ReverseRouter.route(on(PlannedTenderActivityController.class)
            .renderPlannedTenderDetailForm(SCAP_ID, null))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/plannedtender/plannedTenderActivityDetails"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(HasPlannedTenderController.class).renderHasPlannedTenderActivityForm(SCAP_ID))))
        .andExpect(model().attribute("submitPostUrl",
            ReverseRouter.route(on(PlannedTenderActivityController.class)
                .savePlannedTenderDetailForm(SCAP_ID, null, emptyBindingResult()))))
        .andExpect(model().attribute("remunerationModels", RemunerationModel.getRemunerationModels()));
  }

  @Test
  void renderPlannedTenderDetailForm_noScapDetail_expectNotFound() throws Exception {
    when(scapDetailService.getLatestByScap(scap))
        .thenThrow(new ScapEntityNotFoundException("No scap detail found for SCAP with ID [33]"));

    mockMvc.perform(
        get(ReverseRouter.route(on(PlannedTenderActivityController.class)
            .renderPlannedTenderDetailForm(SCAP_ID, null))))
        .andExpect(status().isNotFound());
  }

  @Test
  void renderPlannedTenderDetailForm_noScapPlannedTender_expectNotFound() throws Exception {
    when(scapDetailService.getLatestByScap(scap)).thenReturn(scapDetail);
    when(plannedTenderService.getByScapDetail(scapDetail))
        .thenThrow(new ScapEntityNotFoundException("No scap planned tender found for SCAP with ID [32]"));

    mockMvc.perform(
            get(ReverseRouter.route(on(PlannedTenderActivityController.class)
                .renderPlannedTenderDetailForm(SCAP_ID, null))))
        .andExpect(status().isNotFound());
  }

  @Test
  void savePlannedTenderDetailForm_valid_verifySaves() throws Exception {
    var form = new PlannedTenderActivityForm();
    var expectedRedirectUrl = ReverseRouter.route(on(PlannedTenderController.class)
        .renderPlannedTenderActivities(SCAP_ID));

    when(scapDetailService.getLatestByScap(scap)).thenReturn(scapDetail);
    when(plannedTenderService.getByScapDetail(scapDetail)).thenReturn(plannedTender);
    when(plannedTenderActivityFormService.validate(any(BindingResult.class), eq(form)))
        .thenReturn(new BeanPropertyBindingResult(form, "form"));

    mockMvc.perform(
        post(ReverseRouter.route(on(PlannedTenderActivityController.class)
            .savePlannedTenderDetailForm(SCAP_ID, null, emptyBindingResult())))
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().is3xxRedirection())
        .andExpect(view().name(String.format("redirect:%s", expectedRedirectUrl)));

    verify(plannedTenderActivityService)
        .createPlannedTenderDetail(plannedTender, form);
  }

  @Test
  void savePlannedTenderDetailForm_invalid_verifyNeverSaves() throws Exception {
    var form = new PlannedTenderActivityForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");
    bindingResult.addError(new FieldError("form", "scopeDescription.inputValue", "Required"));

    when(scapDetailService.getLatestByScap(scap)).thenReturn(scapDetail);
    when(plannedTenderService.getByScapDetail(scapDetail)).thenReturn(plannedTender);
    when(plannedTenderActivityFormService.validate(any(BindingResult.class), eq(form)))
        .thenReturn(bindingResult);

    mockMvc.perform(
            post(ReverseRouter.route(on(PlannedTenderActivityController.class)
                .savePlannedTenderDetailForm(SCAP_ID, null, emptyBindingResult())))
                .with(csrf())
                .flashAttr("form", form))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/plannedtender/plannedTenderActivityDetails"))
        .andExpect(model().attributeExists("errorList"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(HasPlannedTenderController.class).renderHasPlannedTenderActivityForm(SCAP_ID))))
        .andExpect(model().attribute("submitPostUrl",
            ReverseRouter.route(on(PlannedTenderActivityController.class)
                .savePlannedTenderDetailForm(SCAP_ID, null, emptyBindingResult()))))
        .andExpect(model().attribute("remunerationModels", RemunerationModel.getRemunerationModels()));

    verify(plannedTenderActivityService, never()).createPlannedTenderDetail(any(), any());
  }

  @Test
  void savePlannedTenderDetailForm_noScapDetail_expectNotFound() throws Exception {
    var form = new PlannedTenderActivityForm();

    when(scapDetailService.getLatestByScap(scap))
        .thenThrow(new ScapEntityNotFoundException("No scap detail found for SCAP with ID [36]"));

    mockMvc.perform(
        post(ReverseRouter.route(on(PlannedTenderActivityController.class)
            .savePlannedTenderDetailForm(SCAP_ID, null, emptyBindingResult())))
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().isNotFound());

    verify(plannedTenderActivityService, never()).createPlannedTenderDetail(any(), any());
  }

  @Test
  void savePlannedTenderDetailForm_noPlannedTender_expectNotFound() throws Exception {
    var form = new PlannedTenderActivityForm();

    when(scapDetailService.getLatestByScap(scap)).thenReturn(scapDetail);
    when(plannedTenderService.getByScapDetail(scapDetail))
        .thenThrow(new ScapEntityNotFoundException("No scap planned tender found for SCAP with ID [37]"));

    mockMvc.perform(
            post(ReverseRouter.route(on(PlannedTenderActivityController.class)
                .savePlannedTenderDetailForm(SCAP_ID, null, emptyBindingResult())))
                .with(csrf())
                .flashAttr("form", form))
        .andExpect(status().isNotFound());

    verify(plannedTenderActivityService, never()).createPlannedTenderDetail(any(), any());
  }
}
