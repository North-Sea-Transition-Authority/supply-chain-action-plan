package uk.co.nstauthority.scap.application.actualtender.detail;

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
import uk.co.nstauthority.scap.application.actualtender.ActualTender;
import uk.co.nstauthority.scap.application.actualtender.ActualTenderService;
import uk.co.nstauthority.scap.application.actualtender.hasactualtender.HasActualTenderController;
import uk.co.nstauthority.scap.application.detail.ScapDetail;
import uk.co.nstauthority.scap.application.detail.ScapDetailService;
import uk.co.nstauthority.scap.application.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.application.overview.ScapOverview;
import uk.co.nstauthority.scap.application.overview.ScapOverviewService;
import uk.co.nstauthority.scap.application.tasklist.TaskListController;
import uk.co.nstauthority.scap.mvc.ReverseRouter;

@ExtendWith(MockitoExtension.class)
@WebMvcTest(ActualTenderDetailController.class)
@WithMockUser
class ActualTenderDetailControllerTest extends AbstractControllerTest {

  @Autowired
  Clock clock;

  @MockBean
  ScapOverviewService scapOverviewService;

  @MockBean
  ScapDetailService scapDetailService;

  @MockBean
  ActualTenderService actualTenderService;

  @MockBean
  ActualTenderDetailFormService actualTenderDetailFormService;

  @MockBean
  ActualTenderDetailService actualTenderDetailService;

  private ScapOverview scap;
  private ScapDetail scapDetail;
  private ActualTender actualTender;

  @BeforeEach
  void setup() {
    scap = new ScapOverview(72);
    scapDetail = new ScapDetail(scap, 1, true, ScapDetailStatus.DRAFT, clock.instant(), 1);
    actualTender = new ActualTender(scapDetail, clock.instant());
  }

  @Test
  void renderActualTenderDetailForm() throws Exception {
    when(scapOverviewService.getScapById(scap.getId())).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(actualTenderService.getByScapDetailOrThrow(scapDetail)).thenReturn(actualTender);

    mockMvc.perform(
        get(ReverseRouter.route(on(ActualTenderDetailController.class)
            .renderActualTenderDetailForm(scap.getId(), null))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/application/actualtender/actualTenderActivityDetail"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(HasActualTenderController.class).renderHasActualTenderForm(scap.getId()))))
        .andExpect(model().attribute("remunerationModels", RemunerationModel.getRemunerationModels()))
        .andExpect(model().attribute("contractStages", ContractStage.getContractStages()));
  }

  @Test
  void saveActualTenderDetailForm_validForm_assertRedirects() throws Exception {
    var form = new ActualTenderDetailForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");
    var expectedRedirectUrl = ReverseRouter.route(on(TaskListController.class).renderTaskList(scap.getId()));

    when(scapOverviewService.getScapById(scap.getId())).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(actualTenderService.getByScapDetailOrThrow(scapDetail)).thenReturn(actualTender);
    when(actualTenderDetailFormService.validate(eq(form), any(BindingResult.class)))
        .thenReturn(bindingResult);

    mockMvc.perform(
        post(ReverseRouter.route(on(ActualTenderDetailController.class)
            .saveActualTenderDetailForm(scap.getId(), null, emptyBindingResult())))
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().is3xxRedirection())
        .andExpect(view().name(String.format("redirect:%s", expectedRedirectUrl)));

    verify(actualTenderDetailService).createActualTenderDetail(actualTender, form);
  }

  @Test
  void saveActualTenderDetailForm_invalidForm_assertOk() throws Exception {
    var form = new ActualTenderDetailForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");
    bindingResult.addError(new FieldError("form", "testField", "Test field must not be blank"));

    when(scapOverviewService.getScapById(scap.getId())).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(actualTenderService.getByScapDetailOrThrow(scapDetail)).thenReturn(actualTender);
    when(actualTenderDetailFormService.validate(eq(form), any(BindingResult.class)))
        .thenReturn(bindingResult);

    mockMvc.perform(
        post(ReverseRouter.route(on(ActualTenderDetailController.class)
            .saveActualTenderDetailForm(scap.getId(), null, emptyBindingResult())))
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/application/actualtender/actualTenderActivityDetail"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(HasActualTenderController.class).renderHasActualTenderForm(scap.getId()))))
        .andExpect(model().attribute("remunerationModels", RemunerationModel.getRemunerationModels()))
        .andExpect(model().attribute("contractStages", ContractStage.getContractStages()))
        .andExpect(model().attributeExists("errorList"));

    verify(actualTenderDetailService, never()).createActualTenderDetail(any(), any());
  }
}
