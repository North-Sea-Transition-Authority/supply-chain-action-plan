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
import uk.co.nstauthority.scap.AbstractScapSubmitterControllerTest;
import uk.co.nstauthority.scap.error.exception.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.RemunerationModel;
import uk.co.nstauthority.scap.scap.plannedtender.PlannedTender;
import uk.co.nstauthority.scap.scap.plannedtender.PlannedTenderController;
import uk.co.nstauthority.scap.scap.plannedtender.PlannedTenderService;
import uk.co.nstauthority.scap.scap.plannedtender.activity.PlannedTenderActivity;
import uk.co.nstauthority.scap.scap.plannedtender.activity.PlannedTenderActivityService;
import uk.co.nstauthority.scap.scap.plannedtender.activity.delete.DeletePlannedTenderActivityController;
import uk.co.nstauthority.scap.scap.plannedtender.hasplannedtender.HasPlannedTenderController;

@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes = DeletePlannedTenderActivityController.class)
@WithMockUser
class DeletePlannedTenderActivityControllerTest extends AbstractScapSubmitterControllerTest {

  @MockBean
  PlannedTenderService plannedTenderService;

  @MockBean
  PlannedTenderActivityService plannedTenderActivityService;

  private PlannedTenderActivity plannedTenderActivity;

  private static final Integer PLANNED_ACTIVITY_ID = 100;

  @BeforeEach
  void setup() {
    plannedTenderActivity = new PlannedTenderActivity(PLANNED_ACTIVITY_ID);
  }

  @Test
  void renderPlannedTenderRemoval_scapDoesNotExist_expectNotFound() throws Exception {
    when(scapService.getScapById(SCAP_ID))
        .thenThrow(new ScapEntityNotFoundException("Could not find SCAP with ID 32"));

    mockMvc.perform(get(
        ReverseRouter.route(on(DeletePlannedTenderActivityController.class)
            .renderPlannedTenderRemoval(SCAP_ID, PLANNED_ACTIVITY_ID))))
        .andExpect(status().isNotFound());
  }

  @Test
  void renderPlannedTenderRemoval_plannedTenderActivityDoesNotExist_expectNotFound() throws Exception {
    when(plannedTenderActivityService.getPlannedTenderDetailById(PLANNED_ACTIVITY_ID))
        .thenThrow(new ScapEntityNotFoundException("Could not find planned tender activity with ID 100"));

    mockMvc.perform(get(
        ReverseRouter.route(on(DeletePlannedTenderActivityController.class)
            .renderPlannedTenderRemoval(SCAP_ID, PLANNED_ACTIVITY_ID))))
        .andExpect(status().isNotFound());
  }

  @Test
  void renderPlannedTenderRemoval_expectIsOk() throws Exception {
    plannedTenderActivity.setAwardRationale("test award rationale");
    plannedTenderActivity.setScopeDescription("test scope description");
    plannedTenderActivity.setEstimatedValue(BigDecimal.valueOf(4.35));
    plannedTenderActivity.setRemunerationModel(RemunerationModel.LUMP_SUM);
    when(plannedTenderActivityService.getPlannedTenderDetailById(100)).thenReturn(plannedTenderActivity);

    mockMvc.perform(get(
        ReverseRouter.route(on(DeletePlannedTenderActivityController.class)
            .renderPlannedTenderRemoval(SCAP_ID, PLANNED_ACTIVITY_ID))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/plannedtender/plannedTenderActivityDelete"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(PlannedTenderController.class).renderPlannedTenderActivities(SCAP_ID))))
        .andExpect(model().attribute("plannedTenderDetail", plannedTenderActivity))
        .andExpect(model().attribute("submitPostUrl",
            ReverseRouter.route(on(DeletePlannedTenderActivityController.class)
                .deletePlannedTenderDetail(SCAP_ID, PLANNED_ACTIVITY_ID, null))));
  }

  @Test
  void deletePlannedTenderDetail_scapDoesNotExist_expectNotFound() throws Exception {
    when(scapService.getScapById(SCAP_ID.scapId()))
        .thenThrow(new ScapEntityNotFoundException("Could not find SCAP with ID %S".formatted(SCAP_ID.scapId())));

    mockMvc.perform(post(
        ReverseRouter.route(on(DeletePlannedTenderActivityController.class)
            .deletePlannedTenderDetail(SCAP_ID, PLANNED_ACTIVITY_ID, null)))
            .with(csrf()))
        .andExpect(status().isNotFound());

    verify(plannedTenderActivityService, never()).deletePlannedTenderDetail(any());
  }

  @Test
  void deletePlannedTenderDetail_plannedTenderActivityDoesNotExist_expectNotFound() throws Exception {
    when(plannedTenderActivityService.getPlannedTenderDetailById(PLANNED_ACTIVITY_ID))
        .thenThrow(new ScapEntityNotFoundException("Could not find planned tender activity with ID 100"));

    mockMvc.perform(post(
        ReverseRouter.route(on(DeletePlannedTenderActivityController.class)
            .deletePlannedTenderDetail(SCAP_ID, PLANNED_ACTIVITY_ID, null)))
            .with(csrf()))
        .andExpect(status().isNotFound());

    verify(plannedTenderActivityService, never()).deletePlannedTenderDetail(any());
  }

  @Test
  void deletePlannedTenderDetail_NoRemainingPlannedTenderActivities_ExpectRedirection() throws Exception {
    var expectedRedirectUrl = ReverseRouter.route(on(HasPlannedTenderController.class)
        .renderHasPlannedTenderActivityForm(SCAP_ID));
    var plannedTender = new PlannedTender();

    when(plannedTenderService.getScapPlannedTenderByScapDetailOrThrow(scapDetail)).thenReturn(plannedTender);
    when(plannedTenderActivityService.getPlannedTenderDetailById(PLANNED_ACTIVITY_ID)).thenReturn(plannedTenderActivity);

    mockMvc.perform(post(
        ReverseRouter.route(on(DeletePlannedTenderActivityController.class)
            .deletePlannedTenderDetail(SCAP_ID, PLANNED_ACTIVITY_ID, null)))
            .with(csrf()))
        .andExpect(status().is3xxRedirection())
        .andExpect(redirectUrl(expectedRedirectUrl));

    verify(plannedTenderActivityService).deletePlannedTenderDetail(plannedTenderActivity);
    verify(plannedTenderService).updatePlannedTenderHasMorePlannedTenders(plannedTender, null);
  }

  @Test
  void deletePlannedTenderDetail_SomeRemainingPlannedTenderActivities_ExpectRedirection() throws Exception {
    var expectedRedirectUrl = ReverseRouter.route(on(PlannedTenderController.class)
        .renderPlannedTenderActivities(SCAP_ID));
    var plannedTender = new PlannedTender();

    when(scapService.getScapById(SCAP_ID)).thenReturn(scap);
    when(plannedTenderService.getScapPlannedTenderByScapDetailOrThrow(scapDetail)).thenReturn(plannedTender);
    when(plannedTenderActivityService.getPlannedTenderDetailById(PLANNED_ACTIVITY_ID)).thenReturn(plannedTenderActivity);
    when(plannedTenderActivityService.hasExistingTenderDetails(plannedTender)).thenReturn(true);

    mockMvc.perform(post(
        ReverseRouter.route(on(DeletePlannedTenderActivityController.class)
            .deletePlannedTenderDetail(SCAP_ID, PLANNED_ACTIVITY_ID, null)))
            .with(csrf()))
        .andExpect(status().is3xxRedirection())
        .andExpect(redirectUrl(expectedRedirectUrl));

    verify(plannedTenderActivityService).deletePlannedTenderDetail(plannedTenderActivity);
  }
}
