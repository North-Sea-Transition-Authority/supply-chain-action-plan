package uk.co.nstauthority.scap.scap.summary;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;

import java.util.Collections;
import org.springframework.test.web.servlet.ResultMatcher;
import org.springframework.web.servlet.ModelAndView;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;

public class ScapSummaryControllerTestUtil {

  private static final ProjectDetailsSummaryView PROJECT_DETAILS_SUMMARY_VIEW = new ProjectDetailsSummaryView(
      null, Collections.emptyList(), null, null, null,
      null, Collections.emptyList(), null, null
  );
  private static final String PROJECT_DETAILS_OBJECT_NAME = "projectDetailsSummaryView";

  public static void mockScapSummaryViewServiceMethods(ScapSummaryViewService scapSummaryViewService, ScapDetail scapDetail) {
    when(scapSummaryViewService.addScapSummaryToModel(any(ModelAndView.class), eq(scapDetail))).thenCallRealMethod();

    when(scapSummaryViewService.getProjectDetailsSummaryView(scapDetail)).thenReturn(PROJECT_DETAILS_SUMMARY_VIEW);
  }

  public static ResultMatcher modelHasSummaryViews() {
    return result -> {
      var model = result.getModelAndView().getModel();
      assertThat(model.get(PROJECT_DETAILS_OBJECT_NAME)).isEqualTo(PROJECT_DETAILS_SUMMARY_VIEW);
    };
  }
}
