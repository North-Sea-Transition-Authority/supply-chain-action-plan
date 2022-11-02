package uk.co.nstauthority.scap.application.plannedtender.delete;

import static org.mockito.ArgumentMatchers.any;
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

import java.math.BigDecimal;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.security.test.context.support.WithMockUser;
import uk.co.nstauthority.scap.AbstractControllerTest;
import uk.co.nstauthority.scap.application.RemunerationModel;
import uk.co.nstauthority.scap.application.overview.ScapOverview;
import uk.co.nstauthority.scap.application.overview.ScapOverviewService;
import uk.co.nstauthority.scap.application.plannedtender.ScapPlannedTenderController;
import uk.co.nstauthority.scap.application.plannedtender.detail.ScapPlannedTenderDetail;
import uk.co.nstauthority.scap.application.plannedtender.detail.ScapPlannedTenderDetailService;
import uk.co.nstauthority.scap.application.plannedtender.detail.delete.DeletePlannedTenderDetailController;
import uk.co.nstauthority.scap.error.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.mvc.ReverseRouter;

@ExtendWith(MockitoExtension.class)
@WebMvcTest(DeletePlannedTenderDetailController.class)
@WithMockUser
class DeletePlannedTenderDetailControllerTest extends AbstractControllerTest {

  @MockBean
  ScapOverviewService scapOverviewService;

  @MockBean
  ScapPlannedTenderDetailService scapPlannedTenderDetailService;

  private ScapOverview scap;
  private ScapPlannedTenderDetail scapPlannedTenderDetail;

  @BeforeEach
  void setup() {
    scap = new ScapOverview(32);
    scapPlannedTenderDetail = new ScapPlannedTenderDetail(100);
  }

  @Test
  void renderPlannedTenderRemoval_scapDoesNotExist_expectNotFound() throws Exception {
    when(scapOverviewService.getScapById(32))
        .thenThrow(new ScapEntityNotFoundException("Could not find SCAP with ID 32"));

    mockMvc.perform(get(
        ReverseRouter.route(on(DeletePlannedTenderDetailController.class)
            .renderPlannedTenderRemoval(32, 100))))
        .andExpect(status().isNotFound());
  }

  @Test
  void renderPlannedTenderRemoval_plannedTenderActivityDoesNotExist_expectNotFound() throws Exception {
    when(scapOverviewService.getScapById(32)).thenReturn(scap);
    when(scapPlannedTenderDetailService.getPlannedTenderDetailById(100))
        .thenThrow(new ScapEntityNotFoundException("Could not find planned tender activity with ID 100"));

    mockMvc.perform(get(
        ReverseRouter.route(on(DeletePlannedTenderDetailController.class)
            .renderPlannedTenderRemoval(32, 100))))
        .andExpect(status().isNotFound());
  }

  @Test
  void renderPlannedTenderRemoval_expectIsOk() throws Exception {
    when(scapOverviewService.getScapById(32)).thenReturn(scap);
    scapPlannedTenderDetail.setAwardRationale("test award rationale");
    scapPlannedTenderDetail.setScopeDescription("test scope description");
    scapPlannedTenderDetail.setEstimatedValue(BigDecimal.valueOf(4.35));
    scapPlannedTenderDetail.setRemunerationModel(RemunerationModel.LUMP_SUM);
    when(scapPlannedTenderDetailService.getPlannedTenderDetailById(100)).thenReturn(scapPlannedTenderDetail);

    mockMvc.perform(get(
        ReverseRouter.route(on(DeletePlannedTenderDetailController.class)
            .renderPlannedTenderRemoval(32, 100))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/application/plannedtender/plannedTenderActivityDelete"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(ScapPlannedTenderController.class).renderPlannedTenderActivities(32))))
        .andExpect(model().attribute("plannedTenderDetail", scapPlannedTenderDetail))
        .andExpect(model().attribute("submitPostUrl",
            ReverseRouter.route(on(DeletePlannedTenderDetailController.class)
                .deletePlannedTenderDetail(32, 100))));
  }

  @Test
  void deletePlannedTenderDetail_scapDoesNotExist_expectNotFound() throws Exception {
    when(scapOverviewService.getScapById(32))
        .thenThrow(new ScapEntityNotFoundException("Could not find SCAP with ID 32"));

    mockMvc.perform(post(
        ReverseRouter.route(on(DeletePlannedTenderDetailController.class)
            .deletePlannedTenderDetail(32, 100)))
            .with(csrf()))
        .andExpect(status().isNotFound());

    verify(scapPlannedTenderDetailService, never()).deletePlannedTenderDetail(any());
  }

  @Test
  void deletePlannedTenderDetail_plannedTenderActivityDoesNotExist_expectNotFound() throws Exception {
    when(scapOverviewService.getScapById(32)).thenReturn(scap);
    when(scapPlannedTenderDetailService.getPlannedTenderDetailById(100))
        .thenThrow(new ScapEntityNotFoundException("Could not find planned tender activity with ID 100"));

    mockMvc.perform(post(
        ReverseRouter.route(on(DeletePlannedTenderDetailController.class)
            .deletePlannedTenderDetail(32, 100)))
            .with(csrf()))
        .andExpect(status().isNotFound());

    verify(scapPlannedTenderDetailService, never()).deletePlannedTenderDetail(any());
  }

  @Test
  void deletePlannedTenderDetail_allOk_expectRedirection() throws Exception {
    var expectedRedirectUrl = ReverseRouter.route(on(ScapPlannedTenderController.class)
        .renderPlannedTenderActivities(32));

    when(scapOverviewService.getScapById(32)).thenReturn(scap);
    when(scapPlannedTenderDetailService.getPlannedTenderDetailById(100)).thenReturn(scapPlannedTenderDetail);

    mockMvc.perform(post(
        ReverseRouter.route(on(DeletePlannedTenderDetailController.class)
            .deletePlannedTenderDetail(32, 100)))
            .with(csrf()))
        .andExpect(status().is3xxRedirection())
        .andExpect(view().name(String.format("redirect:%s", expectedRedirectUrl)));

    verify(scapPlannedTenderDetailService, times(1)).deletePlannedTenderDetail(scapPlannedTenderDetail);
  }
}
