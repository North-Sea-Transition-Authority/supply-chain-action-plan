package uk.co.nstauthority.scap.scap.plannedtender.update;

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
import uk.co.nstauthority.scap.AbstractControllerTest;
import uk.co.nstauthority.scap.error.exception.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.RemunerationModel;
import uk.co.nstauthority.scap.scap.plannedtender.PlannedTenderController;
import uk.co.nstauthority.scap.scap.plannedtender.activity.PlannedTenderActivity;
import uk.co.nstauthority.scap.scap.plannedtender.activity.PlannedTenderActivityForm;
import uk.co.nstauthority.scap.scap.plannedtender.activity.PlannedTenderActivityFormService;
import uk.co.nstauthority.scap.scap.plannedtender.activity.PlannedTenderActivityService;
import uk.co.nstauthority.scap.scap.plannedtender.activity.update.UpdatePlannedTenderDetailController;
import uk.co.nstauthority.scap.scap.scap.Scap;
import uk.co.nstauthority.scap.scap.scap.ScapService;

@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes = UpdatePlannedTenderDetailController.class)
@WithMockUser
class UpdatePlannedTenderDetailControllerTest extends AbstractControllerTest {

  @MockBean
  ScapService scapService;

  @MockBean
  PlannedTenderActivityService plannedTenderActivityService;

  @MockBean
  PlannedTenderActivityFormService plannedTenderActivityFormService;

  private Scap scap;
  private PlannedTenderActivity plannedTenderDetail;

  @BeforeEach
  void setup() {
    scap = new Scap(226);
    plannedTenderDetail = new PlannedTenderActivity(578);
  }

  @Test
  void renderPlannedTenderDetailForm() throws Exception {
    var form = new PlannedTenderActivityForm();

    when(scapService.getScapById(scap.getId())).thenReturn(scap);
    when(plannedTenderActivityService.getPlannedTenderDetailById(plannedTenderDetail.getId()))
        .thenReturn(plannedTenderDetail);
    when(plannedTenderActivityFormService.getForm(plannedTenderDetail)).thenReturn(form);

    mockMvc.perform(
        get(ReverseRouter.route(on(UpdatePlannedTenderDetailController.class)
            .renderUpdatePlannedTenderDetail(scap.getId(), plannedTenderDetail.getId()))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/plannedtender/plannedTenderActivityDetails"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(PlannedTenderController.class)
                .renderPlannedTenderActivities(scap.getId()))))
        .andExpect(model().attribute("remunerationModels", RemunerationModel.getRemunerationModels()))
        .andExpect(model().attribute("form", form));
  }

  @Test
  void renderPlannedTenderDetailForm_noScap_expectNotFound() throws Exception {
    when(scapService.getScapById(scap.getId()))
        .thenThrow(new ScapEntityNotFoundException(
            String.format("No scap found with ID [%d]", scap.getId())));

    mockMvc.perform(
        get(
            ReverseRouter.route(on(UpdatePlannedTenderDetailController.class)
                .renderUpdatePlannedTenderDetail(scap.getId(), plannedTenderDetail.getId()))))
        .andExpect(status().isNotFound());
  }

  @Test
  void renderPlannedTenderDetailForm_noScapPlannedTender_expectNotFound() throws Exception {
    when(scapService.getScapById(scap.getId())).thenReturn(scap);
    when(plannedTenderActivityService.getPlannedTenderDetailById(plannedTenderDetail.getId()))
        .thenThrow(new ScapEntityNotFoundException(
            String.format("No planned tender detail found with ID [%d]", plannedTenderDetail.getId())));

    mockMvc.perform(
        get(
            ReverseRouter.route(on(UpdatePlannedTenderDetailController.class)
                .renderUpdatePlannedTenderDetail(scap.getId(), plannedTenderDetail.getId()))))
        .andExpect(status().isNotFound());
  }

  @Test
  void saveUpdatedPlannedTenderDetail_valid_verifySaves() throws Exception {
    var form = new PlannedTenderActivityForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");
    var expectedRedirectUrl = ReverseRouter.route(on(PlannedTenderController.class)
        .renderPlannedTenderActivities(scap.getId()));

    when(scapService.getScapById(scap.getId())).thenReturn(scap);
    when(plannedTenderActivityService.getPlannedTenderDetailById(plannedTenderDetail.getId()))
        .thenReturn(plannedTenderDetail);
    when(plannedTenderActivityFormService.validate(any(BindingResult.class), eq(form)))
        .thenReturn(bindingResult);

    mockMvc.perform(
        post(ReverseRouter.route(on(UpdatePlannedTenderDetailController.class)
            .saveUpdatedPlannedTenderDetail(scap.getId(), plannedTenderDetail.getId(), null, emptyBindingResult())))
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().is3xxRedirection())
        .andExpect(view().name(String.format("redirect:%s", expectedRedirectUrl)));

    verify(plannedTenderActivityService)
        .updatePlannedTenderDetail(plannedTenderDetail, form);
  }

  @Test
  void saveUpdatedPlannedTenderDetail_invalid_verifyNeverSaves() throws Exception {
    var form = new PlannedTenderActivityForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");
    bindingResult.addError(
        new FieldError("form", "scopeDescription", "This field is required"));

    when(scapService.getScapById(scap.getId())).thenReturn(scap);
    when(plannedTenderActivityService.getPlannedTenderDetailById(plannedTenderDetail.getId()))
        .thenReturn(plannedTenderDetail);
    when(plannedTenderActivityFormService.validate(any(BindingResult.class), eq(form)))
        .thenReturn(bindingResult);

    mockMvc.perform(
        post(ReverseRouter.route(on(UpdatePlannedTenderDetailController.class)
            .saveUpdatedPlannedTenderDetail(scap.getId(), plannedTenderDetail.getId(), null, emptyBindingResult())))
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/plannedtender/plannedTenderActivityDetails"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(PlannedTenderController.class)
                .renderPlannedTenderActivities(scap.getId()))))
        .andExpect(model().attribute("remunerationModels", RemunerationModel.getRemunerationModels()))
        .andExpect(model().attribute("form", form));

    verify(plannedTenderActivityService, never()).updatePlannedTenderDetail(any(), any());
  }

  @Test
  void saveUpdatedPlannedTenderDetail_noPlannedTenderDetail_expectNotFound() throws Exception{
    when(scapService.getScapById(scap.getId())).thenReturn(scap);
    when(plannedTenderActivityService.getPlannedTenderDetailById(plannedTenderDetail.getId()))
        .thenThrow(new ScapEntityNotFoundException(
            String.format("Could not find planned tender detail with ID %d", plannedTenderDetail.getId())));

    mockMvc.perform(
        post(ReverseRouter.route(on(UpdatePlannedTenderDetailController.class)
            .saveUpdatedPlannedTenderDetail(scap.getId(), plannedTenderDetail.getId(), null, emptyBindingResult())))
            .with(csrf()))
        .andExpect(status().isNotFound());

    verify(plannedTenderActivityService, never()).updatePlannedTenderDetail(any(), any());
  }

  @Test
  void saveUpdatedPlannedTenderDetail_noScap_expectNotFound() throws Exception{
    when(scapService.getScapById(scap.getId())).thenThrow(
        new ScapEntityNotFoundException(
            String.format("Could not find SCAP with ID %d", plannedTenderDetail.getId())));

    mockMvc.perform(
            post(ReverseRouter.route(on(UpdatePlannedTenderDetailController.class)
                .saveUpdatedPlannedTenderDetail(scap.getId(), plannedTenderDetail.getId(), null, emptyBindingResult())))
                .with(csrf()))
        .andExpect(status().isNotFound());

    verify(plannedTenderActivityService, never()).updatePlannedTenderDetail(any(), any());
  }
}
