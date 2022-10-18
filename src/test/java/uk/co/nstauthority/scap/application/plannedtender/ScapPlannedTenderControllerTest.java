package uk.co.nstauthority.scap.application.plannedtender;

import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.model;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.view;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.math.BigDecimal;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.security.test.context.support.WithMockUser;
import uk.co.nstauthority.scap.AbstractControllerTest;
import uk.co.nstauthority.scap.application.detail.ScapDetail;
import uk.co.nstauthority.scap.application.detail.ScapDetailService;
import uk.co.nstauthority.scap.application.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.application.overview.ScapOverview;
import uk.co.nstauthority.scap.application.overview.ScapOverviewService;
import uk.co.nstauthority.scap.application.plannedtender.detail.RemunerationModel;
import uk.co.nstauthority.scap.application.plannedtender.detail.ScapPlannedTenderDetail;
import uk.co.nstauthority.scap.application.plannedtender.detail.ScapPlannedTenderDetailService;
import uk.co.nstauthority.scap.application.plannedtender.list.PlannedTenderDetailListItem;
import uk.co.nstauthority.scap.application.plannedtender.list.PlannedTenderDetailListService;
import uk.co.nstauthority.scap.application.tasklist.TaskListController;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.utils.EntityTestingUtil;

@ExtendWith(MockitoExtension.class)
@WebMvcTest(controllers = ScapPlannedTenderController.class)
@WithMockUser
public class ScapPlannedTenderControllerTest extends AbstractControllerTest {

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

  @Test
  public void renderPlannedTenderActivities() throws Exception {
    var scap = new ScapOverview(1664);
    var scapDetail = new ScapDetail(scap, 1, true, ScapDetailStatus.DRAFT, EntityTestingUtil.dateToInstant(2000, 4, 23),
        1);
    var plannedTender = new ScapPlannedTender(scapDetail, EntityTestingUtil.dateToInstant(2000, 4, 23));
    var existingTenderDetails = List.of(
        new ScapPlannedTenderDetail(
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
        new PlannedTenderDetailListItem(existingTenderDetails.get(0), "#", "#")
    );

    when(scapOverviewService.getScapById(22)).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(scapPlannedTenderService.getScapPlannedTenderByScapDetailOrThrow(scapDetail))
        .thenReturn(plannedTender);
    when(scapPlannedTenderDetailService.getTenderDetailsByPlannedTender(plannedTender))
        .thenReturn(existingTenderDetails);
    when(plannedTenderDetailListService.plannedTenderDetailsToListItems(existingTenderDetails))
        .thenReturn(listItems);

    mockMvc.perform(
        get(ReverseRouter.route(on(ScapPlannedTenderController.class).renderPlannedTenderActivities(22))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/application/plannedTender/list"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(22))))
        .andExpect(model().attribute("plannedTenderDetailsList", listItems));
  }
}
