package uk.co.nstauthority.scap.scap.actualtender.hasactualtender;

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

import java.time.Clock;
import java.util.Optional;
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
import uk.co.nstauthority.scap.enumutil.YesNo;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.actualtender.ActualTender;
import uk.co.nstauthority.scap.scap.actualtender.ActualTenderService;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivityController;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivityService;
import uk.co.nstauthority.scap.scap.actualtender.summary.ActualTenderSummaryController;
import uk.co.nstauthority.scap.scap.tasklist.TaskListController;

@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes = HasActualTenderController.class)
@WithMockUser
class HasActualTenderControllerTest extends AbstractScapSubmitterControllerTest {

  @Autowired
  Clock clock;

  @MockBean
  ActualTenderService actualTenderService;

  @MockBean
  HasActualTenderFormService hasActualTenderFormService;

  @MockBean
  ActualTenderActivityService actualTenderActivityService;
  @Test
  void renderHasActualTenderForm_expectIsOk() throws Exception {

    when(scapDetailService.getLatestByScap(scap)).thenReturn(scapDetail);
    when(actualTenderService.findByScapDetail(scapDetail)).thenReturn(Optional.empty());

    mockMvc.perform(
        get(ReverseRouter.route(on(HasActualTenderController.class).renderHasActualTenderForm(scap.getScapId()))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/actualtender/hasActualTender"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(scap.getScapId()))))
        .andExpect(model().attribute("hasActualTender", YesNo.getRadioOptions()))
        .andExpect(model().attributeExists("form"));
  }

  @Test
  void renderHasActualTenderForm_existingActualTender_expectHasForm() throws Exception {
    var actualTender = new ActualTender(scapDetail, clock.instant());
    var form = new HasActualTenderForm();
    form.setHasActualTender(YesNo.NO);

    when(scapService.getScapById(scap.getId())).thenReturn(scap);
    when(actualTenderService.findByScapDetail(scapDetail)).thenReturn(Optional.of(actualTender));
    when(hasActualTenderFormService.getForm(actualTender)).thenReturn(form);

    mockMvc.perform(
            get(ReverseRouter.route(on(HasActualTenderController.class).renderHasActualTenderForm(scap.getScapId()))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/actualtender/hasActualTender"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(scap.getScapId()))))
        .andExpect(model().attribute("hasActualTender", YesNo.getRadioOptions()))
        .andExpect(model().attribute("form", form));
  }

  @Test
  void renderHasActualTenderForm_ExistingActualTenderActivities_AssertRedirects() throws Exception {
    var actualTender = new ActualTender(scapDetail, clock.instant());
    var form = new HasActualTenderForm();
    form.setHasActualTender(YesNo.NO);
    var expectedRedirectUrl = ReverseRouter.route(on(ActualTenderSummaryController.class)
        .renderActualTenderSummary(scap.getScapId()));

    when(scapDetailService.getLatestByScap(scap)).thenReturn(scapDetail);
    when(actualTenderService.findByScapDetail(scapDetail)).thenReturn(Optional.of(actualTender));
    when(hasActualTenderFormService.getForm(actualTender)).thenReturn(form);
    when(actualTenderActivityService.hasActualTenderActivity(actualTender)).thenReturn(true);

    mockMvc.perform(
            get(ReverseRouter.route(on(HasActualTenderController.class).renderHasActualTenderForm(scap.getScapId()))))
        .andExpect(status().is3xxRedirection())
        .andExpect(view().name(String.format("redirect:%s", expectedRedirectUrl)));
  }

  @Test
  void saveHasActualTenderForm_validForm_verifySaves() throws Exception {
    var expectedRedirectUrl = ReverseRouter.route(on(ActualTenderActivityController.class)
        .renderActualTenderActivityForm(scap.getScapId(), null));
    var form = new HasActualTenderForm();
    form.setHasActualTender(YesNo.YES);
    var bindingResult = new BeanPropertyBindingResult(form, "form");

    when(scapService.getScapById(scap.getId())).thenReturn(scap);
    when(actualTenderService.findByScapDetail(scapDetail)).thenReturn(Optional.empty());
    when(hasActualTenderFormService.validate(eq(form), any(BindingResult.class))).thenReturn(bindingResult);

    mockMvc.perform(
        post(ReverseRouter.route(on(HasActualTenderController.class)
            .saveHasActualTenderForm(scap.getScapId(), null, emptyBindingResult())))
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().is3xxRedirection())
        .andExpect(view().name(String.format("redirect:%s", expectedRedirectUrl)));

    verify(actualTenderService).createActualTender(scapDetail, form.getHasActualTender());
  }

  @Test
  void saveHasActualTenderForm_existingActualTender_valid_verifyUpdates() throws Exception {
    var expectedRedirectUrl = ReverseRouter.route(on(TaskListController.class).renderTaskList(scap.getScapId()));
    var form = new HasActualTenderForm();
    form.setHasActualTender(YesNo.NO);
    var bindingResult = new BeanPropertyBindingResult(form, "form");
    var existingActualTender = new ActualTender();

    when(scapService.getScapById(scap.getId())).thenReturn(scap);
    when(actualTenderService.findByScapDetail(scapDetail)).thenReturn(Optional.of(existingActualTender));
    when(hasActualTenderFormService.validate(eq(form), any(BindingResult.class))).thenReturn(bindingResult);

    mockMvc.perform(
        post(ReverseRouter.route(on(HasActualTenderController.class)
            .saveHasActualTenderForm(scap.getScapId(), null, emptyBindingResult())))
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().is3xxRedirection())
        .andExpect(view().name(String.format("redirect:%s", expectedRedirectUrl)));

    verify(actualTenderService).updateHasActualTenders(existingActualTender, YesNo.YES.equals(form.getHasActualTender()));
  }

  @Test
  void saveHasActualTenderForm_invalidForm_verifyNeverSaves() throws Exception {
    var form = new HasActualTenderForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");
    bindingResult.addError(
        new FieldError("form", "hasActualTender", "This field is required")
    );

    when(scapService.getScapById(scap.getId())).thenReturn(scap);
    when(hasActualTenderFormService.validate(eq(form), any(BindingResult.class))).thenReturn(bindingResult);

    mockMvc.perform(
        post(ReverseRouter.route(on(HasActualTenderController.class)
            .saveHasActualTenderForm(scap.getScapId(), null, emptyBindingResult())))
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/actualtender/hasActualTender"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(scap.getScapId()))))
        .andExpect(model().attribute("hasActualTender", YesNo.getRadioOptions()))
        .andExpect(model().attribute("form", form))
        .andExpect(model().attributeExists("errorList"));

    verify(actualTenderService, never()).updateHasActualTenders(any(), any());
  }

  @Test
  void saveHasActualTenderForm_ExistingActualTenderActivities_AssertRedirects() throws Exception {
    var actualTender = new ActualTender(scapDetail, clock.instant());
    var form = new HasActualTenderForm();
    form.setHasActualTender(YesNo.NO);
    var expectedRedirectUrl = ReverseRouter.route(on(ActualTenderSummaryController.class)
        .renderActualTenderSummary(scap.getScapId()));

    when(scapService.getScapById(scap.getId())).thenReturn(scap);
    when(actualTenderService.findByScapDetail(scapDetail)).thenReturn(Optional.of(actualTender));
    when(hasActualTenderFormService.getForm(actualTender)).thenReturn(form);
    when(actualTenderActivityService.hasActualTenderActivity(actualTender)).thenReturn(true);

    mockMvc.perform(
        post(ReverseRouter.route(on(HasActualTenderController.class).renderHasActualTenderForm(scap.getScapId())))
            .with(csrf()))
        .andExpect(status().is3xxRedirection())
        .andExpect(view().name(String.format("redirect:%s", expectedRedirectUrl)));
  }
}
