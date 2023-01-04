package uk.co.nstauthority.scap.scap.plannedtender;

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

import java.math.BigDecimal;
import java.util.Collections;
import java.util.List;
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
import uk.co.nstauthority.scap.AbstractControllerTest;
import uk.co.nstauthority.scap.controllerhelper.ControllerHelperService;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.RemunerationModel;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.plannedtender.activity.PlannedTenderActivity;
import uk.co.nstauthority.scap.scap.plannedtender.activity.PlannedTenderActivityController;
import uk.co.nstauthority.scap.scap.plannedtender.activity.PlannedTenderActivityService;
import uk.co.nstauthority.scap.scap.plannedtender.hasplannedtender.HasPlannedTenderController;
import uk.co.nstauthority.scap.scap.plannedtender.list.PlannedTenderActivityListItem;
import uk.co.nstauthority.scap.scap.plannedtender.list.PlannedTenderActivityListService;
import uk.co.nstauthority.scap.scap.scap.Scap;
import uk.co.nstauthority.scap.scap.scap.ScapService;
import uk.co.nstauthority.scap.scap.tasklist.TaskListController;
import uk.co.nstauthority.scap.utils.EntityTestingUtil;

@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes = PlannedTenderController.class)
@WithMockUser
class PlannedTenderControllerTest extends AbstractControllerTest {

  @MockBean
  ScapDetailService scapDetailService;

  @MockBean
  PlannedTenderService plannedTenderService;

  @MockBean
  PlannedTenderActivityService plannedTenderActivityService;

  @MockBean
  PlannedTenderActivityListService plannedTenderActivityListService;

  @MockBean
  PlannedTenderFormService plannedTenderFormService;

  @Autowired
  ControllerHelperService controllerHelperService;

  private Scap scap;
  private ScapDetail scapDetail;
  private PlannedTender plannedTender;

  @BeforeEach
  void setup() {
    scap = new Scap(22);
    scapDetail = new ScapDetail(
        scap,
        1,
        true,
        ScapDetailStatus.DRAFT,
        EntityTestingUtil.dateToInstant(2000, 4, 23),
        1);
    plannedTender = new PlannedTender(scapDetail, EntityTestingUtil.dateToInstant(2000, 4, 23));
  }

  @Test
  void renderPlannedTenderActivities() throws Exception {
    var existingTenderDetails = List.of(
        new PlannedTenderActivity(
            plannedTender,
            "scope description",
            BigDecimal.valueOf(1.0),
            RemunerationModel.LUMP_SUM,
            null,
            "award rationale",
            EntityTestingUtil.dateToInstant(2000, 4, 23)
        )
    );
    var listItems = List.of(
        new PlannedTenderActivityListItem(existingTenderDetails.get(0), "#", "#")
    );
    var form = new PlannedTenderForm();

    when(scapService.getScapById(scap.getId())).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(plannedTenderService.getScapPlannedTenderByScapDetailOrThrow(scapDetail))
        .thenReturn(plannedTender);
    when(plannedTenderActivityService.getTenderDetailsByPlannedTender(plannedTender))
        .thenReturn(existingTenderDetails);
    when(plannedTenderActivityListService.plannedTenderDetailsToListItems(scap.getId(), existingTenderDetails))
        .thenReturn(listItems);
    when(plannedTenderFormService.getForm(plannedTender)).thenReturn(form);

    mockMvc.perform(
        get(ReverseRouter.route(on(PlannedTenderController.class).renderPlannedTenderActivities(scap.getId()))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/plannedtender/plannedTenderActivityList"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(scap.getId()))))
        .andExpect(model().attribute("plannedTenderDetailsList", listItems))
        .andExpect(model().attribute("form", form))
        .andExpect(model().attribute("radioItems", HasMorePlannedTenderActivities.getRadioItems()));
  }

  @Test
  void renderPlannedTenderActivities_noActivityDetails_expectRedirection() throws Exception {
    var expectedRedirectUrl = ReverseRouter.route(on(HasPlannedTenderController.class)
        .renderHasPlannedTenderActivityForm(scap.getId()));

    when(scapService.getScapById(scap.getId())).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(plannedTenderService.getScapPlannedTenderByScapDetailOrThrow(scapDetail))
        .thenReturn(plannedTender);
    when(plannedTenderActivityService.getTenderDetailsByPlannedTender(plannedTender))
        .thenReturn(Collections.emptyList());

    mockMvc.perform(
        get(ReverseRouter.route(on(PlannedTenderController.class).renderPlannedTenderActivities(scap.getId()))))
        .andExpect(status().is3xxRedirection())
        .andExpect(view().name(String.format("redirect:%s", expectedRedirectUrl)));
  }

  @Test
  void saveAnotherPlannedTenderActivity_yesNow_verifyNeverUpdates() throws Exception {
    var form = new PlannedTenderForm();
    form.setHasMorePlannedTenderActivities(HasMorePlannedTenderActivities.YES_NOW);
    var bindingResult = new BeanPropertyBindingResult(form, "form");
    var expectedRedirectUrl = ReverseRouter.route(on(PlannedTenderActivityController.class)
            .renderPlannedTenderDetailForm(scap.getId(), null));

    when(scapService.getScapById(scap.getId())).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(plannedTenderService.getScapPlannedTenderByScapDetailOrThrow(scapDetail))
        .thenReturn(plannedTender);
    when(plannedTenderActivityService.getTenderDetailsByPlannedTender(plannedTender))
        .thenReturn(Collections.emptyList());
    when(plannedTenderFormService.validate(eq(form), any(BindingResult.class)))
        .thenReturn(bindingResult);

    mockMvc.perform(
        post(
            ReverseRouter.route(on(PlannedTenderController.class)
                .saveAnotherPlannedTenderActivity(scap.getId(), null, emptyBindingResult())))
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().is3xxRedirection())
        .andExpect(view().name(String.format("redirect:%s", expectedRedirectUrl)));

    verify(plannedTenderService, never())
        .updatePlannedTenderHasMorePlannedTenders(any(), any());
  }

  @Test
  void saveAnotherPlannedTenderActivity_yesLater_verifyUpdates() throws Exception {
    var form = new PlannedTenderForm();
    form.setHasMorePlannedTenderActivities(HasMorePlannedTenderActivities.YES_LATER);
    var bindingResult = new BeanPropertyBindingResult(form, "form");
    var expectedRedirectUrl = ReverseRouter.route(on(TaskListController.class).renderTaskList(scap.getId()));

    when(scapService.getScapById(scap.getId())).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(plannedTenderService.getScapPlannedTenderByScapDetailOrThrow(scapDetail))
        .thenReturn(plannedTender);
    when(plannedTenderActivityService.getTenderDetailsByPlannedTender(plannedTender))
        .thenReturn(Collections.emptyList());
    when(plannedTenderFormService.validate(eq(form), any(BindingResult.class)))
        .thenReturn(bindingResult);

    mockMvc.perform(
            post(
                ReverseRouter.route(on(PlannedTenderController.class)
                    .saveAnotherPlannedTenderActivity(scap.getId(), null, emptyBindingResult())))
                .with(csrf())
                .flashAttr("form", form))
        .andExpect(status().is3xxRedirection())
        .andExpect(view().name(String.format("redirect:%s", expectedRedirectUrl)));

    verify(plannedTenderService)
        .updatePlannedTenderHasMorePlannedTenders(plannedTender, form.getHasMorePlannedTenderActivities());
  }

  @Test
  void saveAnotherPlannedTenderActivity_no_verifyUpdates() throws Exception {
    var form = new PlannedTenderForm();
    form.setHasMorePlannedTenderActivities(HasMorePlannedTenderActivities.NO);
    var bindingResult = new BeanPropertyBindingResult(form, "form");
    var expectedRedirectUrl = ReverseRouter.route(on(TaskListController.class).renderTaskList(scap.getId()));

    when(scapService.getScapById(scap.getId())).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(plannedTenderService.getScapPlannedTenderByScapDetailOrThrow(scapDetail))
        .thenReturn(plannedTender);
    when(plannedTenderActivityService.getTenderDetailsByPlannedTender(plannedTender))
        .thenReturn(Collections.emptyList());
    when(plannedTenderFormService.validate(eq(form), any(BindingResult.class)))
        .thenReturn(bindingResult);

    mockMvc.perform(
        post(
            ReverseRouter.route(on(PlannedTenderController.class)
                .saveAnotherPlannedTenderActivity(scap.getId(), null, emptyBindingResult())))
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().is3xxRedirection())
        .andExpect(view().name(String.format("redirect:%s", expectedRedirectUrl)));

    verify(plannedTenderService)
        .updatePlannedTenderHasMorePlannedTenders(plannedTender, form.getHasMorePlannedTenderActivities());
  }

  @Test
  void saveAnotherPlannedTenderActivity_invalid_expectNoRedirect() throws Exception {
    var form = new PlannedTenderForm();
    form.setHasMorePlannedTenderActivities(HasMorePlannedTenderActivities.NO);
    var bindingResult = new BeanPropertyBindingResult(form, "form");
    bindingResult.addError(
        new FieldError("form", "hasMorePlannedTenderActivities", "This field is required"));

    when(scapService.getScapById(scap.getId())).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(plannedTenderService.getScapPlannedTenderByScapDetailOrThrow(scapDetail))
        .thenReturn(plannedTender);
    when(plannedTenderActivityService.getTenderDetailsByPlannedTender(plannedTender))
        .thenReturn(Collections.emptyList());
    when(plannedTenderFormService.validate(eq(form), any(BindingResult.class)))
        .thenReturn(bindingResult);

    mockMvc.perform(
            post(
                ReverseRouter.route(on(PlannedTenderController.class)
                    .saveAnotherPlannedTenderActivity(scap.getId(), null, emptyBindingResult())))
                .with(csrf())
                .flashAttr("form", form))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/plannedtender/plannedTenderActivityList"))
        .andExpect(model().attribute("form", form))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(scap.getId()))))
        .andExpect(model().attribute("plannedTenderDetailsList", Collections.emptyList()))
        .andExpect(model().attribute("radioItems",
            HasMorePlannedTenderActivities.getRadioItems()));

    verify(plannedTenderService, never())
        .updatePlannedTenderHasMorePlannedTenders(any(), any());
  }
}
