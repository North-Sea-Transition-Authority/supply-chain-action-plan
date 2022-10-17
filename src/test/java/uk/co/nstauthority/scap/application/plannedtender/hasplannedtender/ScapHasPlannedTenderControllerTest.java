package uk.co.nstauthority.scap.application.plannedtender.hasplannedtender;

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

import java.util.Optional;
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
import uk.co.nstauthority.scap.application.detail.ScapDetail;
import uk.co.nstauthority.scap.application.detail.ScapDetailService;
import uk.co.nstauthority.scap.application.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.application.overview.ScapOverview;
import uk.co.nstauthority.scap.application.overview.ScapOverviewService;
import uk.co.nstauthority.scap.application.plannedtender.ScapPlannedTender;
import uk.co.nstauthority.scap.application.plannedtender.ScapPlannedTenderController;
import uk.co.nstauthority.scap.application.plannedtender.ScapPlannedTenderService;
import uk.co.nstauthority.scap.application.plannedtender.detail.ScapPlannedTenderDetailController;
import uk.co.nstauthority.scap.application.plannedtender.detail.ScapPlannedTenderDetailService;
import uk.co.nstauthority.scap.application.tasklist.TaskListController;
import uk.co.nstauthority.scap.enumutil.YesNo;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.utils.EntityTestingUtil;

@ExtendWith(MockitoExtension.class)
@WebMvcTest(controllers = ScapHasPlannedTenderController.class)
@WithMockUser
public class ScapHasPlannedTenderControllerTest extends AbstractControllerTest {

  @MockBean
  ScapOverviewService scapOverviewService;

  @MockBean
  ScapDetailService scapDetailService;

  @MockBean
  ScapPlannedTenderService scapPlannedTenderService;

  @MockBean
  ScapPlannedTenderDetailService scapPlannedTenderDetailService;

  @MockBean
  ScapHasPlannedTenderFormService scapHasPlannedTenderFormService;

  private ScapOverview scap;
  private ScapDetail scapDetail;

  @BeforeEach
  void setup() {
    scap = new ScapOverview(1664);
    scapDetail = new ScapDetail(scap, 1, true, ScapDetailStatus.DRAFT, EntityTestingUtil.dateToInstant(2000, 4, 23), 1);
  }

  @Test
  public void renderHasPlannedTenderActivityForm_noExistingEntity() throws Exception {
    var form = new ScapHasPlannedTenderForm();

    when(scapOverviewService.getScapById(22)).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(scapHasPlannedTenderFormService.getForm(scapDetail)).thenReturn(form);
    when(scapPlannedTenderService.getScapPlannedTenderByScapDetail(scapDetail)).thenReturn(Optional.empty());

    mockMvc.perform(
        get(ReverseRouter.route(on(ScapHasPlannedTenderController.class).renderHasPlannedTenderActivityForm(22))))
        .andExpect(status().isOk())
        .andExpect(view().name("/scap/application/plannedTender/hasPlannedTender"))
        .andExpect(model().attribute("form", form))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(22))))
        .andExpect(model().attributeExists("hasPlannedTender"))
        .andExpect(model().attribute("submitPostUrl",
            ReverseRouter.route(on(ScapHasPlannedTenderController.class)
                .saveHasPlannedTenderActivity(22, null, emptyBindingResult()))));
  }

  @Test
  public void saveHasPlannedTenderActivity_yesPlannedTender_verifySave() throws Exception {
    var expectedRedirectUrl = ReverseRouter.route(on(ScapPlannedTenderDetailController.class)
        .renderPlannedTenderDetailForm(22, null));
    var form = new ScapHasPlannedTenderForm();
    form.setHasPlannedTender(YesNo.YES);
    var createdPlannedTender = new ScapPlannedTender(scapDetail, EntityTestingUtil.dateToInstant(2000, 4, 23));

    when(scapOverviewService.getScapById(22)).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(scapPlannedTenderService.getScapPlannedTenderByScapDetail(scapDetail)).thenReturn(Optional.empty());
    when(scapHasPlannedTenderFormService.validate(eq(form), any(BindingResult.class)))
        .thenReturn(emptyBindingResult());
    when(scapPlannedTenderService.createPlannedTenderForScapDetail(scapDetail))
        .thenReturn(createdPlannedTender);

    mockMvc.perform(
        post(ReverseRouter.route(on(ScapHasPlannedTenderController.class)
            .saveHasPlannedTenderActivity(22, null, emptyBindingResult())))
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().is3xxRedirection())
        .andExpect(view().name(String.format("redirect:%s", expectedRedirectUrl)));

    verify(scapPlannedTenderService, times(1)).createPlannedTenderForScapDetail(scapDetail);
    verify(scapPlannedTenderService, times(1))
        .updatePlannedTenderHasPlannedTenders(createdPlannedTender, true);
  }

  @Test
  public void saveHasPlannedTenderActivity_noPlannedTender_verifySaveAndUpdate() throws Exception {
    var expectedRedirectUrl = ReverseRouter.route(on(TaskListController.class).renderTaskList(22));
    var form = new ScapHasPlannedTenderForm();
    form.setHasPlannedTender(YesNo.NO);
    var createdPlannedTender = new ScapPlannedTender(scapDetail, EntityTestingUtil.dateToInstant(2000, 4, 23));

    when(scapOverviewService.getScapById(22)).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(scapPlannedTenderService.getScapPlannedTenderByScapDetail(scapDetail)).thenReturn(Optional.empty());
    when(scapHasPlannedTenderFormService.validate(eq(form), any(BindingResult.class)))
        .thenReturn(emptyBindingResult());
    when(scapPlannedTenderService.createPlannedTenderForScapDetail(scapDetail))
        .thenReturn(createdPlannedTender);

    mockMvc.perform(
            post(ReverseRouter.route(on(ScapHasPlannedTenderController.class)
                .saveHasPlannedTenderActivity(22, null, emptyBindingResult())))
                .with(csrf())
                .flashAttr("form", form))
        .andExpect(status().is3xxRedirection())
        .andExpect(view().name(String.format("redirect:%s", expectedRedirectUrl)));

    verify(scapPlannedTenderService, times(1)).createPlannedTenderForScapDetail(scapDetail);
    verify(scapPlannedTenderService, times(1))
        .updatePlannedTenderHasPlannedTenders(createdPlannedTender, false);
  }

  @Test
  public void saveHasPlannedTenderActivity_invalidForm_verifyNoSave() throws Exception {
    var form = new ScapHasPlannedTenderForm();
    form.setHasPlannedTender(null);
    var bindingResult = new BeanPropertyBindingResult(form, "form");
    bindingResult.addError(new FieldError("form", "hasPlannedTender", "This field is required"));

    when(scapOverviewService.getScapById(22)).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(scapPlannedTenderService.getScapPlannedTenderByScapDetail(scapDetail)).thenReturn(Optional.empty());
    when(scapHasPlannedTenderFormService.validate(eq(form), any(BindingResult.class)))
        .thenReturn(bindingResult);

    mockMvc.perform(
        post(ReverseRouter.route(on(ScapHasPlannedTenderController.class)
            .saveHasPlannedTenderActivity(22, null, emptyBindingResult())))
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().isOk())
        .andExpect(view().name("/scap/application/plannedTender/hasPlannedTender"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(22))))
        .andExpect(model().attributeExists("hasPlannedTender"))
        .andExpect(model().attribute("form", form))
        .andExpect(model().attribute("submitPostUrl",
            ReverseRouter.route(on(ScapHasPlannedTenderController.class)
                .saveHasPlannedTenderActivity(22, null, emptyBindingResult()))));

    verify(scapPlannedTenderService, never()).createPlannedTenderForScapDetail(any());
  }

  @Test
  public void saveHasPlannedTenderActivity_existingPlannedTenders_expectRedirect() throws Exception {
    var form = new ScapHasPlannedTenderForm();
    form.setHasPlannedTender(null);
    var existingPlannedTender = new ScapPlannedTender(scapDetail, EntityTestingUtil.dateToInstant(2000, 4, 23));
    var expectedRedirectUrl = ReverseRouter.route(on(ScapPlannedTenderController.class)
        .renderPlannedTenderActivities(22));

    when(scapOverviewService.getScapById(22)).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(scapPlannedTenderService.getScapPlannedTenderByScapDetail(scapDetail))
        .thenReturn(Optional.of(existingPlannedTender));
    when(scapPlannedTenderDetailService.hasExistingTenderDetails(existingPlannedTender)).thenReturn(true);

    mockMvc.perform(
        post(ReverseRouter.route(on(ScapHasPlannedTenderController.class)
            .saveHasPlannedTenderActivity(22, null, emptyBindingResult())))
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().is3xxRedirection())
        .andExpect(view().name(String.format("redirect:%s", expectedRedirectUrl)));

    verify(scapPlannedTenderService, never()).createPlannedTenderForScapDetail(any());
  }
}
