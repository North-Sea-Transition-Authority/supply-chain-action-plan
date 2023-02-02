package uk.co.nstauthority.scap.workarea;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.csrf;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.model;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.redirectedUrl;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.view;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ContextConfiguration;
import uk.co.nstauthority.scap.AbstractControllerTest;
import uk.co.nstauthority.scap.authentication.ServiceUserDetail;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.start.ScapStartController;
import uk.co.nstauthority.scap.scap.summary.ScapSubmissionStage;

@SuppressWarnings("ConstantConditions")
@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes = WorkAreaController.class)
@WithMockUser
class WorkAreaControllerTest extends AbstractControllerTest {

  @MockBean
  WorkAreaService workAreaService;

  @MockBean
  WorkAreaFilterService workAreaFilterService;

  private List<WorkAreaItem> workAreaItems;
  private Map<String, String> statusCheckboxes;

  @BeforeEach
  void setup() {
    workAreaItems = List.of(
        new WorkAreaItem(
            new ScapId(1),
            1,
            "ref",
            "operator",
            "projectName",
            ScapDetailStatus.DRAFT,
            ScapSubmissionStage.CONTRACTING_STRATEGY_PENDING,
            false
        ));
    statusCheckboxes = ScapDetailStatus.getRadioOptions();

    when(userDetailService.getUserDetail()).thenReturn(testUser);
    when(workAreaService.getWorkAreaItems(eq(testUser), any(WorkAreaFilter.class))).thenReturn(workAreaItems);
  }

  @Test
  void getWorkArea() throws Exception {
    mockMvc.perform(
        get(ReverseRouter.route(on(WorkAreaController.class).getWorkArea(null))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/workarea/workArea"))
        .andExpect(model().attribute("startScapUrl",
            ReverseRouter.route(on(ScapStartController.class).renderStartNewScap())))
        .andExpect(model().attribute("workAreaItems", workAreaItems))
        .andExpect(model().attribute("statusCheckboxes", statusCheckboxes))
        .andExpect(model().attributeExists("form"));

  }

  @Test
  void teamMemberNotScapSubmitter_cannotStartScap() throws Exception {
    when(teamMemberService.getAllPermissionsForUser(any(ServiceUserDetail.class)))
        .thenReturn(List.of(RolePermission.VIEW_SCAP));

    mockMvc.perform(
        get(ReverseRouter.route(on(WorkAreaController.class).getWorkArea(null))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/workarea/workArea"))
        .andExpect(model().attribute("startScapUrl",
            ReverseRouter.route(on(ScapStartController.class).renderStartNewScap())))
        .andExpect(model().attribute("canStartScap", false))
        .andExpect(model().attribute("statusCheckboxes", statusCheckboxes))
        .andExpect(model().attributeExists("form"));
  }

  @Test
  void teamMemberScapSubmitter_canStartScap() throws Exception {
    when(teamMemberService.getAllPermissionsForUser(any(ServiceUserDetail.class)))
        .thenReturn(List.of(RolePermission.SUBMIT_SCAP));

    mockMvc.perform(
        get(ReverseRouter.route(on(WorkAreaController.class).getWorkArea(null))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/workarea/workArea"))
        .andExpect(model().attribute("startScapUrl",
            ReverseRouter.route(on(ScapStartController.class).renderStartNewScap())))
        .andExpect(model().attribute("canStartScap", true))
        .andExpect(model().attribute("statusCheckboxes", statusCheckboxes))
        .andExpect(model().attributeExists("form"));
  }

  @Test
  void filterWorkArea() throws Exception {
    var form = new WorkAreaForm();
    form.setScapStatuses(Collections.singletonList(ScapDetailStatus.DRAFT));
    var filter = new WorkAreaFilter();
    var expectedRedirectUrl = ReverseRouter.route(on(WorkAreaController.class).getWorkArea(null));

    mockMvc.perform(
        post(ReverseRouter.route(on(WorkAreaController.class).filterWorkArea(null, null)))
            .with(csrf())
            .flashAttr("form", form)
            .flashAttr("workAreaFilter", filter))
        .andExpect(status().is3xxRedirection())
        .andExpect(redirectedUrl(expectedRedirectUrl));

    assertThat(filter)
        .extracting(WorkAreaFilter::getScapStatuses)
        .isEqualTo(form.getScapStatuses());
  }

  @Test
  void clearWorkAreaFilter() throws Exception {
    var form = new WorkAreaForm();
    form.setScapStatuses(Collections.singletonList(ScapDetailStatus.DRAFT));
    var filter = new WorkAreaFilter();
    filter.update(form);

    mockMvc.perform(
        get(ReverseRouter.route(on(WorkAreaController.class).clearWorkAreaFilter(null, null)))
            .flashAttr("workAreaFilter", filter))
        .andExpect(status().is3xxRedirection())
        .andExpect(redirectedUrl(ReverseRouter.route(on(WorkAreaController.class).getWorkArea(null))));

    assertThat(filter).extracting(
        WorkAreaFilter::getScapStatuses
    ).isNull();
  }
}
