package uk.co.nstauthority.scap.application.plannedtender;

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
import uk.co.nstauthority.scap.application.plannedtender.detail.ScapPlannedTenderDetail;
import uk.co.nstauthority.scap.application.plannedtender.detail.ScapPlannedTenderDetailController;
import uk.co.nstauthority.scap.application.plannedtender.detail.ScapPlannedTenderDetailService;
import uk.co.nstauthority.scap.application.plannedtender.hasplannedtender.ScapHasPlannedTenderController;
import uk.co.nstauthority.scap.application.plannedtender.list.PlannedTenderDetailListItem;
import uk.co.nstauthority.scap.application.plannedtender.list.PlannedTenderDetailListService;
import uk.co.nstauthority.scap.application.tasklist.TaskListController;
import uk.co.nstauthority.scap.controllerhelper.ControllerHelperService;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.utils.EntityTestingUtil;

@ExtendWith(MockitoExtension.class)
@WebMvcTest(controllers = ScapPlannedTenderController.class)
@WithMockUser
class ScapPlannedTenderControllerTest extends AbstractControllerTest {

  @MockBean
  ScapOverviewService scapOverviewService;

  @MockBean
  ScapDetailService scapDetailService;

  @MockBean
  ScapPlannedTenderService scapPlannedTenderService;

  @MockBean
  ScapPlannedTenderDetailService scapPlannedTenderDetailService;

  @MockBean
  PlannedTenderDetailListService plannedTenderDetailListService;

  @MockBean
  ScapPlannedTenderFormService scapPlannedTenderFormService;

  @Autowired
  ControllerHelperService controllerHelperService;

  private ScapOverview scap;
  private ScapDetail scapDetail;
  private ScapPlannedTender scapPlannedTender;

  @BeforeEach
  void setup() {
    scap = new ScapOverview(22);
    scapDetail = new ScapDetail(
        scap,
        1,
        true,
        ScapDetailStatus.DRAFT,
        EntityTestingUtil.dateToInstant(2000, 4, 23),
        1);
    scapPlannedTender = new ScapPlannedTender(scapDetail, EntityTestingUtil.dateToInstant(2000, 4, 23));
  }

  @Test
  void renderPlannedTenderActivities() throws Exception {
    var existingTenderDetails = List.of(
        new ScapPlannedTenderDetail(
            scapPlannedTender,
            "scope description",
            BigDecimal.valueOf(1.0),
            RemunerationModel.LUMP_SUM,
            null,
            "award rationale",
            EntityTestingUtil.dateToInstant(2000, 4, 23)
        )
    );
    var listItems = List.of(
        new PlannedTenderDetailListItem(existingTenderDetails.get(0), "#", "#")
    );
    var form = new ScapPlannedTenderForm();

    when(scapOverviewService.getScapById(scap.getId())).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(scapPlannedTenderService.getScapPlannedTenderByScapDetailOrThrow(scapDetail))
        .thenReturn(scapPlannedTender);
    when(scapPlannedTenderDetailService.getTenderDetailsByPlannedTender(scapPlannedTender))
        .thenReturn(existingTenderDetails);
    when(plannedTenderDetailListService.plannedTenderDetailsToListItems(scap.getId(), existingTenderDetails))
        .thenReturn(listItems);
    when(scapPlannedTenderFormService.getForm(scapPlannedTender)).thenReturn(form);

    mockMvc.perform(
        get(ReverseRouter.route(on(ScapPlannedTenderController.class).renderPlannedTenderActivities(scap.getId()))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/application/plannedtender/plannedTenderActivityList"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(scap.getId()))))
        .andExpect(model().attribute("plannedTenderDetailsList", listItems))
        .andExpect(model().attribute("form", form))
        .andExpect(model().attribute("radioItems", HasMorePlannedTenderActivities.getRadioItems()));
  }

  @Test
  void renderPlannedTenderActivities_noActivityDetails_expectRedirection() throws Exception {
    var expectedRedirectUrl = ReverseRouter.route(on(ScapHasPlannedTenderController.class)
        .renderHasPlannedTenderActivityForm(scap.getId()));

    when(scapOverviewService.getScapById(scap.getId())).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(scapPlannedTenderService.getScapPlannedTenderByScapDetailOrThrow(scapDetail))
        .thenReturn(scapPlannedTender);
    when(scapPlannedTenderDetailService.getTenderDetailsByPlannedTender(scapPlannedTender))
        .thenReturn(Collections.emptyList());

    mockMvc.perform(
        get(ReverseRouter.route(on(ScapPlannedTenderController.class).renderPlannedTenderActivities(scap.getId()))))
        .andExpect(status().is3xxRedirection())
        .andExpect(view().name(String.format("redirect:%s", expectedRedirectUrl)));
  }

  @Test
  void saveAnotherPlannedTenderActivity_yesNow_verifyNeverUpdates() throws Exception {
    var form = new ScapPlannedTenderForm();
    form.setHasMorePlannedTenderActivities(HasMorePlannedTenderActivities.YES_NOW);
    var bindingResult = new BeanPropertyBindingResult(form, "form");
    var expectedRedirectUrl = ReverseRouter.route(on(ScapPlannedTenderDetailController.class)
            .renderPlannedTenderDetailForm(scap.getId(), null));

    when(scapOverviewService.getScapById(scap.getId())).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(scapPlannedTenderService.getScapPlannedTenderByScapDetailOrThrow(scapDetail))
        .thenReturn(scapPlannedTender);
    when(scapPlannedTenderDetailService.getTenderDetailsByPlannedTender(scapPlannedTender))
        .thenReturn(Collections.emptyList());
    when(scapPlannedTenderFormService.validate(eq(form), any(BindingResult.class)))
        .thenReturn(bindingResult);

    mockMvc.perform(
        post(
            ReverseRouter.route(on(ScapPlannedTenderController.class)
                .saveAnotherPlannedTenderActivity(scap.getId(), null, emptyBindingResult())))
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().is3xxRedirection())
        .andExpect(view().name(String.format("redirect:%s", expectedRedirectUrl)));

    verify(scapPlannedTenderService, never())
        .updatePlannedTenderHasMorePlannedTenders(any(), any());
  }

  @Test
  void saveAnotherPlannedTenderActivity_yesLater_verifyUpdates() throws Exception {
    var form = new ScapPlannedTenderForm();
    form.setHasMorePlannedTenderActivities(HasMorePlannedTenderActivities.YES_LATER);
    var bindingResult = new BeanPropertyBindingResult(form, "form");
    var expectedRedirectUrl = ReverseRouter.route(on(TaskListController.class).renderTaskList(scap.getId()));

    when(scapOverviewService.getScapById(scap.getId())).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(scapPlannedTenderService.getScapPlannedTenderByScapDetailOrThrow(scapDetail))
        .thenReturn(scapPlannedTender);
    when(scapPlannedTenderDetailService.getTenderDetailsByPlannedTender(scapPlannedTender))
        .thenReturn(Collections.emptyList());
    when(scapPlannedTenderFormService.validate(eq(form), any(BindingResult.class)))
        .thenReturn(bindingResult);

    mockMvc.perform(
            post(
                ReverseRouter.route(on(ScapPlannedTenderController.class)
                    .saveAnotherPlannedTenderActivity(scap.getId(), null, emptyBindingResult())))
                .with(csrf())
                .flashAttr("form", form))
        .andExpect(status().is3xxRedirection())
        .andExpect(view().name(String.format("redirect:%s", expectedRedirectUrl)));

    verify(scapPlannedTenderService)
        .updatePlannedTenderHasMorePlannedTenders(scapPlannedTender, form.getHasMorePlannedTenderActivities());
  }

  @Test
  void saveAnotherPlannedTenderActivity_no_verifyUpdates() throws Exception {
    var form = new ScapPlannedTenderForm();
    form.setHasMorePlannedTenderActivities(HasMorePlannedTenderActivities.NO);
    var bindingResult = new BeanPropertyBindingResult(form, "form");
    var expectedRedirectUrl = ReverseRouter.route(on(TaskListController.class).renderTaskList(scap.getId()));

    when(scapOverviewService.getScapById(scap.getId())).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(scapPlannedTenderService.getScapPlannedTenderByScapDetailOrThrow(scapDetail))
        .thenReturn(scapPlannedTender);
    when(scapPlannedTenderDetailService.getTenderDetailsByPlannedTender(scapPlannedTender))
        .thenReturn(Collections.emptyList());
    when(scapPlannedTenderFormService.validate(eq(form), any(BindingResult.class)))
        .thenReturn(bindingResult);

    mockMvc.perform(
        post(
            ReverseRouter.route(on(ScapPlannedTenderController.class)
                .saveAnotherPlannedTenderActivity(scap.getId(), null, emptyBindingResult())))
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().is3xxRedirection())
        .andExpect(view().name(String.format("redirect:%s", expectedRedirectUrl)));

    verify(scapPlannedTenderService)
        .updatePlannedTenderHasMorePlannedTenders(scapPlannedTender, form.getHasMorePlannedTenderActivities());
  }

  @Test
  void saveAnotherPlannedTenderActivity_invalid_expectNoRedirect() throws Exception {
    var form = new ScapPlannedTenderForm();
    form.setHasMorePlannedTenderActivities(HasMorePlannedTenderActivities.NO);
    var bindingResult = new BeanPropertyBindingResult(form, "form");
    bindingResult.addError(
        new FieldError("form", "hasMorePlannedTenderActivities", "This field is required"));

    when(scapOverviewService.getScapById(scap.getId())).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(scapPlannedTenderService.getScapPlannedTenderByScapDetailOrThrow(scapDetail))
        .thenReturn(scapPlannedTender);
    when(scapPlannedTenderDetailService.getTenderDetailsByPlannedTender(scapPlannedTender))
        .thenReturn(Collections.emptyList());
    when(scapPlannedTenderFormService.validate(eq(form), any(BindingResult.class)))
        .thenReturn(bindingResult);

    mockMvc.perform(
            post(
                ReverseRouter.route(on(ScapPlannedTenderController.class)
                    .saveAnotherPlannedTenderActivity(scap.getId(), null, emptyBindingResult())))
                .with(csrf())
                .flashAttr("form", form))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/application/plannedtender/plannedTenderActivityList"))
        .andExpect(model().attribute("form", form))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(scap.getId()))))
        .andExpect(model().attribute("plannedTenderDetailsList", Collections.emptyList()))
        .andExpect(model().attribute("radioItems",
            HasMorePlannedTenderActivities.getRadioItems()));

    verify(scapPlannedTenderService, never())
        .updatePlannedTenderHasMorePlannedTenders(any(), any());
  }
}
