package uk.co.nstauthority.scap.scap.plannedtender.hasplannedtender;

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

import java.util.Optional;
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
import uk.co.nstauthority.scap.scap.plannedtender.PlannedTender;
import uk.co.nstauthority.scap.scap.plannedtender.PlannedTenderController;
import uk.co.nstauthority.scap.scap.plannedtender.PlannedTenderService;
import uk.co.nstauthority.scap.scap.plannedtender.activity.PlannedTenderActivityController;
import uk.co.nstauthority.scap.scap.plannedtender.activity.PlannedTenderActivityService;
import uk.co.nstauthority.scap.scap.tasklist.TaskListController;
import uk.co.nstauthority.scap.utils.EntityTestingUtil;
import uk.co.nstauthority.scap.validation.ValidationErrorOrderingService;

@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes = HasPlannedTenderController.class)
@WithMockUser
class HasPlannedTenderControllerTest extends AbstractScapSubmitterControllerTest {

@MockBean
  PlannedTenderService plannedTenderService;

  @MockBean
  PlannedTenderActivityService plannedTenderActivityService;

  @MockBean
  HasPlannedTenderFormService hasPlannedTenderFormService;

  @MockBean
  ValidationErrorOrderingService validationErrorOrderingService;

  @Test
  void renderHasPlannedTenderActivityForm_noExistingEntity() throws Exception {
    var form = new HasPlannedTenderForm();

    when(hasPlannedTenderFormService.getForm(scapDetail)).thenReturn(form);
    when(plannedTenderService.findByScapDetail(scapDetail)).thenReturn(Optional.empty());

    mockMvc.perform(
        get(ReverseRouter.route(on(HasPlannedTenderController.class).renderHasPlannedTenderActivityForm(SCAP_ID))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/plannedtender/hasPlannedTender"))
        .andExpect(model().attribute("form", form))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(SCAP_ID))))
        .andExpect(model().attributeExists("hasPlannedTender"))
        .andExpect(model().attribute("submitPostUrl",
            ReverseRouter.route(on(HasPlannedTenderController.class)
                .saveHasPlannedTenderActivity(SCAP_ID, null, emptyBindingResult()))));
  }

  @Test
  void saveHasPlannedTenderActivity_yesPlannedTender_verifySave() throws Exception {
    var expectedRedirectUrl = ReverseRouter.route(on(PlannedTenderActivityController.class)
        .renderPlannedTenderDetailForm(SCAP_ID, null));
    var form = new HasPlannedTenderForm();
    form.setHasPlannedTender(YesNo.YES);
    var createdPlannedTender = new PlannedTender(scapDetail, EntityTestingUtil.dateToInstant(2000, 4, 23));

    when(plannedTenderService.findByScapDetail(scapDetail)).thenReturn(Optional.empty());
    when(hasPlannedTenderFormService.validate(eq(form), any(BindingResult.class)))
        .thenReturn(emptyBindingResult());
    when(plannedTenderService.createPlannedTenderForScapDetail(scapDetail))
        .thenReturn(createdPlannedTender);

    mockMvc.perform(
        post(ReverseRouter.route(on(HasPlannedTenderController.class)
            .saveHasPlannedTenderActivity(SCAP_ID, null, emptyBindingResult())))
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().is3xxRedirection())
        .andExpect(view().name(String.format("redirect:%s", expectedRedirectUrl)));

    verify(plannedTenderService).createPlannedTenderForScapDetail(scapDetail);
    verify(plannedTenderService)
        .updatePlannedTenderHasPlannedTenders(createdPlannedTender, true);
  }

  @Test
  void saveHasPlannedTenderActivity_noPlannedTender_verifySaveAndUpdate() throws Exception {
    var expectedRedirectUrl = ReverseRouter.route(on(TaskListController.class).renderTaskList(SCAP_ID));
    var form = new HasPlannedTenderForm();
    form.setHasPlannedTender(YesNo.NO);
    var createdPlannedTender = new PlannedTender(scapDetail, EntityTestingUtil.dateToInstant(2000, 4, 23));

    when(plannedTenderService.findByScapDetail(scapDetail)).thenReturn(Optional.empty());
    when(hasPlannedTenderFormService.validate(eq(form), any(BindingResult.class)))
        .thenReturn(emptyBindingResult());
    when(plannedTenderService.createPlannedTenderForScapDetail(scapDetail))
        .thenReturn(createdPlannedTender);

    mockMvc.perform(
            post(ReverseRouter.route(on(HasPlannedTenderController.class)
                .saveHasPlannedTenderActivity(SCAP_ID, null, emptyBindingResult())))
                .with(csrf())
                .flashAttr("form", form))
        .andExpect(status().is3xxRedirection())
        .andExpect(view().name(String.format("redirect:%s", expectedRedirectUrl)));

    verify(plannedTenderService).createPlannedTenderForScapDetail(scapDetail);
    verify(plannedTenderService)
        .updatePlannedTenderHasPlannedTenders(createdPlannedTender, false);
  }

  @Test
  void saveHasPlannedTenderActivity_invalidForm_verifyNoSave() throws Exception {
    var form = new HasPlannedTenderForm();
    form.setHasPlannedTender(null);
    var bindingResult = new BeanPropertyBindingResult(form, "form");
    bindingResult.addError(new FieldError("form", "hasPlannedTender", "This field is required"));

    when(plannedTenderService.findByScapDetail(scapDetail)).thenReturn(Optional.empty());
    when(hasPlannedTenderFormService.validate(eq(form), any(BindingResult.class)))
        .thenReturn(bindingResult);

    mockMvc.perform(
        post(ReverseRouter.route(on(HasPlannedTenderController.class)
            .saveHasPlannedTenderActivity(SCAP_ID, null, emptyBindingResult())))
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/plannedtender/hasPlannedTender"))
        .andExpect(model().attributeExists("errorItems"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(SCAP_ID))))
        .andExpect(model().attributeExists("hasPlannedTender"))
        .andExpect(model().attribute("form", form))
        .andExpect(model().attribute("submitPostUrl",
            ReverseRouter.route(on(HasPlannedTenderController.class)
                .saveHasPlannedTenderActivity(SCAP_ID, null, emptyBindingResult()))));

    verify(plannedTenderService, never()).createPlannedTenderForScapDetail(any());
  }

  @Test
  void saveHasPlannedTenderActivity_existingPlannedTenders_expectRedirect() throws Exception {
    var form = new HasPlannedTenderForm();
    form.setHasPlannedTender(null);
    var existingPlannedTender = new PlannedTender(scapDetail, EntityTestingUtil.dateToInstant(2000, 4, 23));
    var expectedRedirectUrl = ReverseRouter.route(on(PlannedTenderController.class)
        .renderPlannedTenderActivities(SCAP_ID));

    when(plannedTenderService.findByScapDetail(scapDetail))
        .thenReturn(Optional.of(existingPlannedTender));
    when(plannedTenderActivityService.hasExistingTenderDetails(existingPlannedTender)).thenReturn(true);

    mockMvc.perform(
        post(ReverseRouter.route(on(HasPlannedTenderController.class)
            .saveHasPlannedTenderActivity(SCAP_ID, null, emptyBindingResult())))
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().is3xxRedirection())
        .andExpect(view().name(String.format("redirect:%s", expectedRedirectUrl)));

    verify(plannedTenderService, never()).createPlannedTenderForScapDetail(any());
  }
}
