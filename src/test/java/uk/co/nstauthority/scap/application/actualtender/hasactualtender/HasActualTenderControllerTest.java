package uk.co.nstauthority.scap.application.actualtender.hasactualtender;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
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
import uk.co.nstauthority.scap.application.overview.ScapOverview;
import uk.co.nstauthority.scap.application.overview.ScapOverviewService;
import uk.co.nstauthority.scap.application.tasklist.TaskListController;
import uk.co.nstauthority.scap.enumutil.YesNo;
import uk.co.nstauthority.scap.mvc.ReverseRouter;

@ExtendWith(MockitoExtension.class)
@WebMvcTest(HasActualTenderController.class)
@WithMockUser
class HasActualTenderControllerTest extends AbstractControllerTest {

  @MockBean
  ScapOverviewService scapOverviewService;

  @MockBean
  HasActualTenderFormService hasActualTenderFormService;

  private ScapOverview scap;

  @BeforeEach
  void setup() {
    scap = new ScapOverview(42);
  }

  @Test
  void renderHasActualTenderForm_expectIsOk() throws Exception {
    var form = new HasActualTenderForm();

    when(scapOverviewService.getScapById(scap.getId())).thenReturn(scap);
    when(hasActualTenderFormService.getForm()).thenReturn(form);

    mockMvc.perform(
        get(ReverseRouter.route(on(HasActualTenderController.class).renderHasActualTenderForm(scap.getId()))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/application/actualtender/hasActualTender"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(scap.getId()))))
        .andExpect(model().attribute("hasActualTender", YesNo.getRadioOptions()))
        .andExpect(model().attribute("form", form));
  }

  @Test
  void saveHasActualTenderForm_validForm_verifySaves() throws Exception {
    var expectedRedirectUrl = ReverseRouter.route(on(TaskListController.class).renderTaskList(scap.getId()));
    var form = new HasActualTenderForm();
    form.setHasActualTender(YesNo.YES);
    var bindingResult = new BeanPropertyBindingResult(form, "form");

    when(scapOverviewService.getScapById(scap.getId())).thenReturn(scap);
    when(hasActualTenderFormService.validate(eq(form), any(BindingResult.class))).thenReturn(bindingResult);

    mockMvc.perform(
        post(ReverseRouter.route(on(HasActualTenderController.class)
            .saveHasActualTenderForm(scap.getId(), null, emptyBindingResult())))
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().is3xxRedirection())
        .andExpect(view().name(String.format("redirect:%s", expectedRedirectUrl)));

    // TODO: When adding data model, verify that valid form gets saved
  }

  @Test
  void saveHasActualTenderForm_invalidForm_verifyNeverSaves() throws Exception {
    var form = new HasActualTenderForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");
    bindingResult.addError(
        new FieldError("form", "hasActualTender", "This field is required")
    );

    when(scapOverviewService.getScapById(scap.getId())).thenReturn(scap);
    when(hasActualTenderFormService.validate(eq(form), any(BindingResult.class))).thenReturn(bindingResult);

    mockMvc.perform(
            post(ReverseRouter.route(on(HasActualTenderController.class)
                .saveHasActualTenderForm(scap.getId(), null, emptyBindingResult())))
                .with(csrf())
                .flashAttr("form", form))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/application/actualtender/hasActualTender"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(scap.getId()))))
        .andExpect(model().attribute("hasActualTender", YesNo.getRadioOptions()))
        .andExpect(model().attribute("form", form))
        .andExpect(model().attributeExists("errorList"));

    // TODO: When adding data model, verify that invalid form does not get saved
  }
}
