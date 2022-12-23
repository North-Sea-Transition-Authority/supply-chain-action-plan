package uk.co.nstauthority.scap.scap.summary;

import static java.util.Map.entry;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;

import java.util.Collections;
import org.springframework.test.web.servlet.ResultMatcher;
import org.springframework.web.servlet.ModelAndView;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.summary.actualtender.ActualTenderSummaryView;
import uk.co.nstauthority.scap.scap.summary.plannedtender.PlannedTenderSummaryView;

public class ScapSummaryControllerTestUtil {

  private static final ProjectDetailsSummaryView PROJECT_DETAILS_SUMMARY_VIEW = new ProjectDetailsSummaryView(
      null, Collections.emptyList(), null, null, null,
      null, Collections.emptyList(), null, null
  );
  private static final PlannedTenderSummaryView PLANNED_TENDER_SUMMARY_VIEW = new PlannedTenderSummaryView(
      null,
      null
  );
  private static final ActualTenderSummaryView ACTUAL_TENDER_SUMMARY_VIEW = new ActualTenderSummaryView(
      null, null
  );

  public static void mockScapSummaryViewServiceMethods(ScapSummaryViewService scapSummaryViewService, ScapDetail scapDetail) {
    when(scapSummaryViewService.addScapSummaryToModel(any(ModelAndView.class), eq(scapDetail))).thenCallRealMethod();

    when(scapSummaryViewService.getProjectDetailsSummaryView(scapDetail)).thenReturn(PROJECT_DETAILS_SUMMARY_VIEW);
    when(scapSummaryViewService.getPlannedTenderSummaryView(scapDetail)).thenReturn(PLANNED_TENDER_SUMMARY_VIEW);
    when(scapSummaryViewService.getActualTenderSummaryView(scapDetail)).thenReturn(ACTUAL_TENDER_SUMMARY_VIEW);
  }

  public static ResultMatcher modelHasSummaryViews() {
    return result -> {
      var modelAndView = result.getModelAndView();
      assertThat(modelAndView).isNotNull();
      assertThat(modelAndView.getModel()).contains(
          entry(ScapSummaryViewService.PROJECT_DETAILS_OBJECT_NAME, PROJECT_DETAILS_SUMMARY_VIEW),
          entry(ScapSummaryViewService.PLANNED_TENDER_OBJECT_NAME, PLANNED_TENDER_SUMMARY_VIEW),
          entry(ScapSummaryViewService.ACTUAL_TENDER_OBJECT_NAME, ACTUAL_TENDER_SUMMARY_VIEW)
      );
    };
  }
}
