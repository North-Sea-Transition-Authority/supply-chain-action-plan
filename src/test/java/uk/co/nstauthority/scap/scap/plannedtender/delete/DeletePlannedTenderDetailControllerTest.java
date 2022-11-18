package uk.co.nstauthority.scap.scap.plannedtender.delete;

import static org.mockito.ArgumentMatchers.any;
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
import static uk.co.nstauthority.scap.utils.ControllerTestingUtil.redirectUrl;

import java.math.BigDecimal;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ContextConfiguration;
import uk.co.nstauthority.scap.AbstractControllerTest;
import uk.co.nstauthority.scap.error.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.RemunerationModel;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.plannedtender.PlannedTender;
import uk.co.nstauthority.scap.scap.plannedtender.PlannedTenderController;
import uk.co.nstauthority.scap.scap.plannedtender.PlannedTenderService;
import uk.co.nstauthority.scap.scap.plannedtender.activity.PlannedTenderActivity;
import uk.co.nstauthority.scap.scap.plannedtender.activity.PlannedTenderActivityService;
import uk.co.nstauthority.scap.scap.plannedtender.activity.delete.DeletePlannedTenderDetailController;
import uk.co.nstauthority.scap.scap.plannedtender.hasplannedtender.HasPlannedTenderController;
import uk.co.nstauthority.scap.scap.scap.Scap;
import uk.co.nstauthority.scap.scap.scap.ScapService;

@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes = DeletePlannedTenderDetailController.class)
@WithMockUser
class DeletePlannedTenderDetailControllerTest extends AbstractControllerTest {

  @MockBean
  ScapService scapService;

  @MockBean
  ScapDetailService scapDetailService;

  @MockBean
  PlannedTenderService plannedTenderService;

  @MockBean
  PlannedTenderActivityService plannedTenderActivityService;

  private Scap scap;
  private PlannedTenderActivity plannedTenderActivity;

  @BeforeEach
  void setup() {
    scap = new Scap(32);
    plannedTenderActivity = new PlannedTenderActivity(100);
  }

  @Test
  void renderPlannedTenderRemoval_scapDoesNotExist_expectNotFound() throws Exception {
    when(scapService.getScapById(32))
        .thenThrow(new ScapEntityNotFoundException("Could not find SCAP with ID 32"));

    mockMvc.perform(get(
        ReverseRouter.route(on(DeletePlannedTenderDetailController.class)
            .renderPlannedTenderRemoval(32, 100))))
        .andExpect(status().isNotFound());
  }

  @Test
  void renderPlannedTenderRemoval_plannedTenderActivityDoesNotExist_expectNotFound() throws Exception {
    when(scapService.getScapById(32)).thenReturn(scap);
    when(plannedTenderActivityService.getPlannedTenderDetailById(100))
        .thenThrow(new ScapEntityNotFoundException("Could not find planned tender activity with ID 100"));

    mockMvc.perform(get(
        ReverseRouter.route(on(DeletePlannedTenderDetailController.class)
            .renderPlannedTenderRemoval(32, 100))))
        .andExpect(status().isNotFound());
  }

  @Test
  void renderPlannedTenderRemoval_expectIsOk() throws Exception {
    when(scapService.getScapById(32)).thenReturn(scap);
    plannedTenderActivity.setAwardRationale("test award rationale");
    plannedTenderActivity.setScopeDescription("test scope description");
    plannedTenderActivity.setEstimatedValue(BigDecimal.valueOf(4.35));
    plannedTenderActivity.setRemunerationModel(RemunerationModel.LUMP_SUM);
    when(plannedTenderActivityService.getPlannedTenderDetailById(100)).thenReturn(plannedTenderActivity);

    mockMvc.perform(get(
        ReverseRouter.route(on(DeletePlannedTenderDetailController.class)
            .renderPlannedTenderRemoval(32, 100))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/plannedtender/plannedTenderActivityDelete"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(PlannedTenderController.class).renderPlannedTenderActivities(32))))
        .andExpect(model().attribute("plannedTenderDetail", plannedTenderActivity))
        .andExpect(model().attribute("submitPostUrl",
            ReverseRouter.route(on(DeletePlannedTenderDetailController.class)
                .deletePlannedTenderDetail(32, 100, null))));
  }

  @Test
  void deletePlannedTenderDetail_scapDoesNotExist_expectNotFound() throws Exception {
    when(scapService.getScapById(32))
        .thenThrow(new ScapEntityNotFoundException("Could not find SCAP with ID 32"));

    mockMvc.perform(post(
        ReverseRouter.route(on(DeletePlannedTenderDetailController.class)
            .deletePlannedTenderDetail(32, 100, null)))
            .with(csrf()))
        .andExpect(status().isNotFound());

    verify(plannedTenderActivityService, never()).deletePlannedTenderDetail(any());
  }

  @Test
  void deletePlannedTenderDetail_plannedTenderActivityDoesNotExist_expectNotFound() throws Exception {
    when(scapService.getScapById(32)).thenReturn(scap);
    when(plannedTenderActivityService.getPlannedTenderDetailById(100))
        .thenThrow(new ScapEntityNotFoundException("Could not find planned tender activity with ID 100"));

    mockMvc.perform(post(
        ReverseRouter.route(on(DeletePlannedTenderDetailController.class)
            .deletePlannedTenderDetail(32, 100, null)))
            .with(csrf()))
        .andExpect(status().isNotFound());

    verify(plannedTenderActivityService, never()).deletePlannedTenderDetail(any());
  }

  @Test
  void deletePlannedTenderDetail_NoRemainingPlannedTenderActivities_ExpectRedirection() throws Exception {
    var expectedRedirectUrl = ReverseRouter.route(on(HasPlannedTenderController.class)
        .renderHasPlannedTenderActivityForm(32));

    when(scapService.getScapById(32)).thenReturn(scap);
    when(plannedTenderActivityService.getPlannedTenderDetailById(100)).thenReturn(plannedTenderActivity);

    mockMvc.perform(post(
        ReverseRouter.route(on(DeletePlannedTenderDetailController.class)
            .deletePlannedTenderDetail(32, 100, null)))
            .with(csrf()))
        .andExpect(status().is3xxRedirection())
        .andExpect(redirectUrl(expectedRedirectUrl));

    verify(plannedTenderActivityService).deletePlannedTenderDetail(plannedTenderActivity);
  }

  @Test
  void deletePlannedTenderDetail_SomeRemainingPlannedTenderActivities_ExpectRedirection() throws Exception {
    var expectedRedirectUrl = ReverseRouter.route(on(PlannedTenderController.class)
        .renderPlannedTenderActivities(32));
    var scapDetail = new ScapDetail();
    var plannedTender = new PlannedTender();

    when(scapService.getScapById(32)).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(plannedTenderService.getScapPlannedTenderByScapDetailOrThrow(scapDetail)).thenReturn(plannedTender);
    when(plannedTenderActivityService.getPlannedTenderDetailById(100)).thenReturn(plannedTenderActivity);
    when(plannedTenderActivityService.hasExistingTenderDetails(plannedTender)).thenReturn(true);

    mockMvc.perform(post(
        ReverseRouter.route(on(DeletePlannedTenderDetailController.class)
            .deletePlannedTenderDetail(32, 100, null)))
            .with(csrf()))
        .andExpect(status().is3xxRedirection())
        .andExpect(redirectUrl(expectedRedirectUrl));

    verify(plannedTenderActivityService).deletePlannedTenderDetail(plannedTenderActivity);
  }
}
