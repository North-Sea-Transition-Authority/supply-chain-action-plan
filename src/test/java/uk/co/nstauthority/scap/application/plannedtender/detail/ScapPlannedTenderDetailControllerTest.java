package uk.co.nstauthority.scap.application.plannedtender.detail;

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
import uk.co.nstauthority.scap.application.detail.ScapDetail;
import uk.co.nstauthority.scap.application.detail.ScapDetailService;
import uk.co.nstauthority.scap.application.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.application.overview.ScapOverview;
import uk.co.nstauthority.scap.application.overview.ScapOverviewService;
import uk.co.nstauthority.scap.application.plannedtender.ScapPlannedTender;
import uk.co.nstauthority.scap.application.plannedtender.ScapPlannedTenderController;
import uk.co.nstauthority.scap.application.plannedtender.ScapPlannedTenderService;
import uk.co.nstauthority.scap.application.plannedtender.hasplannedtender.ScapHasPlannedTenderController;
import uk.co.nstauthority.scap.error.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.utils.EntityTestingUtil;
import uk.co.nstauthority.scap.validation.ValidationErrorOrderingService;

@ExtendWith(MockitoExtension.class)
@WebMvcTest(controllers = ScapPlannedTenderDetailController.class)
@WithMockUser
class ScapPlannedTenderDetailControllerTest extends AbstractControllerTest {


  @MockBean
  ScapOverviewService scapOverviewService;

  @MockBean
  ScapDetailService scapDetailService;

  @MockBean
  ScapPlannedTenderService scapPlannedTenderService;

  @MockBean
  ScapPlannedTenderDetailService scapPlannedTenderDetailService;

  @MockBean
  ScapPlannedTenderDetailFormService scapPlannedTenderDetailFormService;

  @MockBean
  ValidationErrorOrderingService validationErrorOrderingService;

  private ScapOverview scap;
  private ScapDetail scapDetail;
  private ScapPlannedTender scapPlannedTender;

  @BeforeEach
  void setup() {
    scap = new ScapOverview(1664);
    scapDetail = new ScapDetail(scap, 1, true, ScapDetailStatus.DRAFT, EntityTestingUtil.dateToInstant(2000, 4, 23), 1);
    scapPlannedTender = new ScapPlannedTender(scapDetail, EntityTestingUtil.dateToInstant(2000, 4, 23));
  }

  @Test
  void renderPlannedTenderDetailForm() throws Exception {
    when(scapOverviewService.getScapById(32)).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(scapPlannedTenderService.getScapPlannedTenderByScapDetailOrThrow(scapDetail)).thenReturn(scapPlannedTender);
    when(scapPlannedTenderDetailService.hasExistingTenderDetails(scapPlannedTender)).thenReturn(false);

    mockMvc.perform(
        get(ReverseRouter.route(on(ScapPlannedTenderDetailController.class)
            .renderPlannedTenderDetailForm(32, null))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/application/plannedtender/plannedTenderActivityDetail"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(ScapHasPlannedTenderController.class).renderHasPlannedTenderActivityForm(32))))
        .andExpect(model().attribute("submitPostUrl",
            ReverseRouter.route(on(ScapPlannedTenderDetailController.class)
                .savePlannedTenderDetailForm(32, null, emptyBindingResult()))))
        .andExpect(model().attribute("remunerationModels", RemunerationModel.getRemunerationModels()));
  }

  @Test
  void renderPlannedTenderDetailForm_noScapDetail_expectNotFound() throws Exception {
    when(scapOverviewService.getScapById(33)).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap))
        .thenThrow(new ScapEntityNotFoundException("No scap detail found for SCAP with ID [33]"));

    mockMvc.perform(
        get(ReverseRouter.route(on(ScapPlannedTenderDetailController.class)
            .renderPlannedTenderDetailForm(33, null))))
        .andExpect(status().isNotFound());
  }

  @Test
  void renderPlannedTenderDetailForm_noScapPlannedTender_expectNotFound() throws Exception {
    when(scapOverviewService.getScapById(34)).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(scapPlannedTenderService.getScapPlannedTenderByScapDetailOrThrow(scapDetail))
        .thenThrow(new ScapEntityNotFoundException("No scap planned tender found for SCAP with ID [34]"));

    mockMvc.perform(
            get(ReverseRouter.route(on(ScapPlannedTenderDetailController.class)
                .renderPlannedTenderDetailForm(34, null))))
        .andExpect(status().isNotFound());
  }

  @Test
  void savePlannedTenderDetailForm_valid_verifySaves() throws Exception {
    var form = new ScapPlannedTenderDetailForm();
    var expectedRedirectUrl = ReverseRouter.route(on(ScapPlannedTenderController.class)
        .renderPlannedTenderActivities(34));

    when(scapOverviewService.getScapById(34)).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(scapPlannedTenderService.getScapPlannedTenderByScapDetailOrThrow(scapDetail)).thenReturn(scapPlannedTender);
    when(scapPlannedTenderDetailFormService.validate(any(BindingResult.class), eq(form)))
        .thenReturn(new BeanPropertyBindingResult(form, "form"));

    mockMvc.perform(
        post(ReverseRouter.route(on(ScapPlannedTenderDetailController.class)
            .savePlannedTenderDetailForm(34, null, emptyBindingResult())))
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().is3xxRedirection())
        .andExpect(view().name(String.format("redirect:%s", expectedRedirectUrl)));

    verify(scapPlannedTenderDetailService, times(1))
        .createPlannedTenderDetail(scapPlannedTender, form);
  }

  @Test
  void savePlannedTenderDetailForm_invalid_verifyNeverSaves() throws Exception {
    var form = new ScapPlannedTenderDetailForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");
    bindingResult.addError(new FieldError("form", "scopeDescription.inputValue", "Required"));

    when(scapOverviewService.getScapById(35)).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(scapPlannedTenderService.getScapPlannedTenderByScapDetailOrThrow(scapDetail)).thenReturn(scapPlannedTender);
    when(scapPlannedTenderDetailFormService.validate(any(BindingResult.class), eq(form)))
        .thenReturn(bindingResult);

    mockMvc.perform(
            post(ReverseRouter.route(on(ScapPlannedTenderDetailController.class)
                .savePlannedTenderDetailForm(35, null, emptyBindingResult())))
                .with(csrf())
                .flashAttr("form", form))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/application/plannedtender/plannedTenderActivityDetail"))
        .andExpect(model().attributeExists("errorItems"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(ScapHasPlannedTenderController.class).renderHasPlannedTenderActivityForm(35))))
        .andExpect(model().attribute("submitPostUrl",
            ReverseRouter.route(on(ScapPlannedTenderDetailController.class)
                .savePlannedTenderDetailForm(35, null, emptyBindingResult()))))
        .andExpect(model().attribute("remunerationModels", RemunerationModel.getRemunerationModels()));

    verify(scapPlannedTenderDetailService, never()).createPlannedTenderDetail(any(), any());
  }

  @Test
  void savePlannedTenderDetailForm_noScapDetail_expectNotFound() throws Exception {
    var form = new ScapPlannedTenderDetailForm();

    when(scapOverviewService.getScapById(36)).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap))
        .thenThrow(new ScapEntityNotFoundException("No scap detail found for SCAP with ID [36]"));

    mockMvc.perform(
        post(ReverseRouter.route(on(ScapPlannedTenderDetailController.class)
            .savePlannedTenderDetailForm(36, null, emptyBindingResult())))
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().isNotFound());

    verify(scapPlannedTenderDetailService, never()).createPlannedTenderDetail(any(), any());
  }

  @Test
  void savePlannedTenderDetailForm_noPlannedTender_expectNotFound() throws Exception {
    var form = new ScapPlannedTenderDetailForm();

    when(scapOverviewService.getScapById(37)).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(scapPlannedTenderService.getScapPlannedTenderByScapDetailOrThrow(scapDetail))
        .thenThrow(new ScapEntityNotFoundException("No scap planned tender found for SCAP with ID [37]"));

    mockMvc.perform(
            post(ReverseRouter.route(on(ScapPlannedTenderDetailController.class)
                .savePlannedTenderDetailForm(37, null, emptyBindingResult())))
                .with(csrf())
                .flashAttr("form", form))
        .andExpect(status().isNotFound());

    verify(scapPlannedTenderDetailService, never()).createPlannedTenderDetail(any(), any());
  }
}
