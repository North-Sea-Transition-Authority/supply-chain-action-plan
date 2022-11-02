package uk.co.nstauthority.scap.application.plannedtender.update;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
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
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.validation.BeanPropertyBindingResult;
import org.springframework.validation.BindingResult;
import org.springframework.validation.FieldError;
import uk.co.nstauthority.scap.AbstractControllerTest;
import uk.co.nstauthority.scap.application.RemunerationModel;
import uk.co.nstauthority.scap.application.overview.ScapOverview;
import uk.co.nstauthority.scap.application.overview.ScapOverviewService;
import uk.co.nstauthority.scap.application.plannedtender.ScapPlannedTenderController;
import uk.co.nstauthority.scap.application.plannedtender.detail.ScapPlannedTenderDetail;
import uk.co.nstauthority.scap.application.plannedtender.detail.ScapPlannedTenderDetailForm;
import uk.co.nstauthority.scap.application.plannedtender.detail.ScapPlannedTenderDetailFormService;
import uk.co.nstauthority.scap.application.plannedtender.detail.ScapPlannedTenderDetailService;
import uk.co.nstauthority.scap.application.plannedtender.detail.update.UpdatePlannedTenderDetailController;
import uk.co.nstauthority.scap.error.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.mvc.ReverseRouter;

@ExtendWith(MockitoExtension.class)
@WebMvcTest(UpdatePlannedTenderDetailController.class)
@WithMockUser
class UpdatePlannedTenderDetailControllerTest extends AbstractControllerTest {

  @MockBean
  ScapOverviewService scapOverviewService;

  @MockBean
  ScapPlannedTenderDetailService scapPlannedTenderDetailService;

  @MockBean
  ScapPlannedTenderDetailFormService scapPlannedTenderDetailFormService;

  private ScapOverview scap;
  private ScapPlannedTenderDetail plannedTenderDetail;

  @BeforeEach
  void setup() {
    scap = new ScapOverview(226);
    plannedTenderDetail = new ScapPlannedTenderDetail(578);
  }

  @Test
  void renderPlannedTenderDetailForm() throws Exception {
    var form = new ScapPlannedTenderDetailForm();

    when(scapOverviewService.getScapById(scap.getId())).thenReturn(scap);
    when(scapPlannedTenderDetailService.getPlannedTenderDetailById(plannedTenderDetail.getId()))
        .thenReturn(plannedTenderDetail);
    when(scapPlannedTenderDetailFormService.getForm(plannedTenderDetail)).thenReturn(form);

    mockMvc.perform(
        get(ReverseRouter.route(on(UpdatePlannedTenderDetailController.class)
            .renderUpdatePlannedTenderDetail(scap.getId(), plannedTenderDetail.getId()))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/application/plannedtender/plannedTenderActivityDetail"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(ScapPlannedTenderController.class)
                .renderPlannedTenderActivities(scap.getId()))))
        .andExpect(model().attribute("remunerationModels", RemunerationModel.getRemunerationModels()))
        .andExpect(model().attribute("form", form));
  }

  @Test
  void renderPlannedTenderDetailForm_noScap_expectNotFound() throws Exception {
    when(scapOverviewService.getScapById(scap.getId()))
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
    when(scapOverviewService.getScapById(scap.getId())).thenReturn(scap);
    when(scapPlannedTenderDetailService.getPlannedTenderDetailById(plannedTenderDetail.getId()))
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
    var form = new ScapPlannedTenderDetailForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");
    var expectedRedirectUrl = ReverseRouter.route(on(ScapPlannedTenderController.class)
        .renderPlannedTenderActivities(scap.getId()));

    when(scapOverviewService.getScapById(scap.getId())).thenReturn(scap);
    when(scapPlannedTenderDetailService.getPlannedTenderDetailById(plannedTenderDetail.getId()))
        .thenReturn(plannedTenderDetail);
    when(scapPlannedTenderDetailFormService.validate(any(BindingResult.class), eq(form)))
        .thenReturn(bindingResult);

    mockMvc.perform(
        post(ReverseRouter.route(on(UpdatePlannedTenderDetailController.class)
            .saveUpdatedPlannedTenderDetail(scap.getId(), plannedTenderDetail.getId(), null, emptyBindingResult())))
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().is3xxRedirection())
        .andExpect(view().name(String.format("redirect:%s", expectedRedirectUrl)));

    verify(scapPlannedTenderDetailService, times(1))
        .updatePlannedTenderDetail(plannedTenderDetail, form);
  }

  @Test
  void saveUpdatedPlannedTenderDetail_invalid_verifyNeverSaves() throws Exception {
    var form = new ScapPlannedTenderDetailForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");
    bindingResult.addError(
        new FieldError("form", "scopeDescription", "This field is required"));

    when(scapOverviewService.getScapById(scap.getId())).thenReturn(scap);
    when(scapPlannedTenderDetailService.getPlannedTenderDetailById(plannedTenderDetail.getId()))
        .thenReturn(plannedTenderDetail);
    when(scapPlannedTenderDetailFormService.validate(any(BindingResult.class), eq(form)))
        .thenReturn(bindingResult);

    mockMvc.perform(
        post(ReverseRouter.route(on(UpdatePlannedTenderDetailController.class)
            .saveUpdatedPlannedTenderDetail(scap.getId(), plannedTenderDetail.getId(), null, emptyBindingResult())))
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/application/plannedtender/plannedTenderActivityDetail"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(ScapPlannedTenderController.class)
                .renderPlannedTenderActivities(scap.getId()))))
        .andExpect(model().attribute("remunerationModels", RemunerationModel.getRemunerationModels()))
        .andExpect(model().attribute("form", form));

    verify(scapPlannedTenderDetailService, never()).updatePlannedTenderDetail(any(), any());
  }

  @Test
  void saveUpdatedPlannedTenderDetail_noPlannedTenderDetail_expectNotFound() throws Exception{
    when(scapOverviewService.getScapById(scap.getId())).thenReturn(scap);
    when(scapPlannedTenderDetailService.getPlannedTenderDetailById(plannedTenderDetail.getId()))
        .thenThrow(new ScapEntityNotFoundException(
            String.format("Could not find planned tender detail with ID %d", plannedTenderDetail.getId())));

    mockMvc.perform(
        post(ReverseRouter.route(on(UpdatePlannedTenderDetailController.class)
            .saveUpdatedPlannedTenderDetail(scap.getId(), plannedTenderDetail.getId(), null, emptyBindingResult())))
            .with(csrf()))
        .andExpect(status().isNotFound());

    verify(scapPlannedTenderDetailService, never()).updatePlannedTenderDetail(any(), any());
  }

  @Test
  void saveUpdatedPlannedTenderDetail_noScap_expectNotFound() throws Exception{
    when(scapOverviewService.getScapById(scap.getId())).thenThrow(
        new ScapEntityNotFoundException(
            String.format("Could not find SCAP with ID %d", plannedTenderDetail.getId())));

    mockMvc.perform(
            post(ReverseRouter.route(on(UpdatePlannedTenderDetailController.class)
                .saveUpdatedPlannedTenderDetail(scap.getId(), plannedTenderDetail.getId(), null, emptyBindingResult())))
                .with(csrf()))
        .andExpect(status().isNotFound());

    verify(scapPlannedTenderDetailService, never()).updatePlannedTenderDetail(any(), any());
  }
}
