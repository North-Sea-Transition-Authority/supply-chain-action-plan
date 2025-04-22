package uk.co.nstauthority.scap.scap.pathfinder;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.csrf;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.model;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.redirectedUrl;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.view;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.time.Instant;
import java.util.Collections;
import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.validation.BindingResult;
import uk.co.nstauthority.scap.AbstractScapSubmitterControllerTest;
import uk.co.nstauthority.scap.energyportal.PathfinderProjectService;
import uk.co.nstauthority.scap.energyportal.rest.PathfinderRestController;
import uk.co.nstauthority.scap.fds.addtolist.AddToListItem;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.tasklist.TaskListController;
import uk.co.nstauthority.scap.utils.ValidatorTestingUtil;

@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes = PathfinderController.class)
class PathfinderControllerTest extends AbstractScapSubmitterControllerTest {

  @MockBean
  private PathfinderService pathfinderService;

  @MockBean
  private PathfinderFormValidator pathfinderFormValidator;

  @MockBean
  private PathfinderProjectService pathfinderProjectService;

  @Test
  void renderPathfinderProjectsForm_NoPathfinderProjectsOverview() throws Exception {
    when(scapDetailService.getLatestByScapId(SCAP_ID)).thenReturn(scapDetail);
    when(pathfinderService.findPathfinderProjectsOverview(scapDetail)).thenReturn(Optional.empty());

    var modelAndView = mockMvc.perform(get(ReverseRouter.route(on(PathfinderController.class).renderPathfinderProjectsForm(SCAP_ID)))
        .with(authenticatedScapUser()))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/pathfinder"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(SCAP_ID))))
        .andExpect(model().attribute("pathfinderSearchRestUrl",
            ReverseRouter.route(on(PathfinderRestController.class).getPathfinderSearchResults(SCAP_ID, null))))
        .andReturn()
        .getModelAndView();

    assertThat(modelAndView).isNotNull();
    assertThat((PathfinderForm) modelAndView.getModel().get("form")).extracting(
        PathfinderForm::getHasPathfinderProjects,
        form -> form.getNoPathfinderProjectRationale().getInputValue(),
        PathfinderForm::getPathfinderProjectIds
    ).containsOnlyNulls();
  }

  @Test
  void renderPathfinderProjectsForm_HasNoPathfinderProjects() throws Exception {
    var pathfinderProjectsOverview = new PathfinderProjectsOverview(scapDetail, Instant.now());
    pathfinderProjectsOverview.setHasRelatedPathfinderProjects(false);
    pathfinderProjectsOverview.setNoPathfinderProjectsRationale("some rationale");

    when(scapDetailService.getLatestByScapId(SCAP_ID)).thenReturn(scapDetail);
    when(pathfinderService.findPathfinderProjectsOverview(scapDetail)).thenReturn(Optional.of(pathfinderProjectsOverview));

    var modelAndView = mockMvc.perform(get(ReverseRouter.route(on(PathfinderController.class).renderPathfinderProjectsForm(SCAP_ID)))
            .with(authenticatedScapUser()))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/pathfinder"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(SCAP_ID))))
        .andExpect(model().attribute("pathfinderSearchRestUrl",
            ReverseRouter.route(on(PathfinderRestController.class).getPathfinderSearchResults(SCAP_ID, null))))
        .andReturn()
        .getModelAndView();

    assertThat(modelAndView).isNotNull();
    assertThat((PathfinderForm) modelAndView.getModel().get("form")).extracting(
        PathfinderForm::getHasPathfinderProjects,
        form -> form.getNoPathfinderProjectRationale().getInputValue(),
        PathfinderForm::getPathfinderProjectIds
    ).containsExactly(
        pathfinderProjectsOverview.getHasRelatedPathfinderProjects(),
        pathfinderProjectsOverview.getNoPathfinderProjectsRationale(),
        Collections.emptyList()
    );
  }

  @Test
  void renderPathfinderProjectsForm_HasPathfinderProjects() throws Exception {
    var pathfinderProjectsOverview = new PathfinderProjectsOverview(scapDetail, Instant.now());
    pathfinderProjectsOverview.setHasRelatedPathfinderProjects(true);
    var pathfinderProject = new PathfinderProject(
        pathfinderProjectsOverview,
        374,
        "Some Pathfinder project title",
        Instant.now()
    );
    var projectIds = Collections.singletonList(pathfinderProject.getPathfinderProjectId());
    var addToListItems = Collections.singletonList(
        new AddToListItem(
            pathfinderProject.getPathfinderProjectId().toString(),
            pathfinderProject.getPathfinderProjectName(),
            true
        )
    );

    when(scapDetailService.getLatestByScapId(SCAP_ID)).thenReturn(scapDetail);
    when(pathfinderService.findPathfinderProjectsOverview(scapDetail)).thenReturn(Optional.of(pathfinderProjectsOverview));
    when(pathfinderService.findAllByPathfinderProjectsOverview(pathfinderProjectsOverview))
        .thenReturn(Collections.singleton(pathfinderProject));
    when(pathfinderProjectService.getProjectAddToListItems(projectIds)).thenReturn(addToListItems);

    var modelAndView = mockMvc.perform(get(ReverseRouter.route(on(PathfinderController.class).renderPathfinderProjectsForm(SCAP_ID)))
            .with(authenticatedScapUser()))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/pathfinder"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(SCAP_ID))))
        .andExpect(model().attribute("pathfinderSearchRestUrl",
            ReverseRouter.route(on(PathfinderRestController.class).getPathfinderSearchResults(SCAP_ID, null))))
        .andExpect(model().attribute("preselectedProjects", addToListItems))
        .andReturn()
        .getModelAndView();

    assertThat(modelAndView).isNotNull();
    assertThat((PathfinderForm) modelAndView.getModel().get("form")).extracting(
        PathfinderForm::getHasPathfinderProjects,
        form -> form.getNoPathfinderProjectRationale().getInputValue(),
        PathfinderForm::getPathfinderProjectIds
    ).containsExactly(
        pathfinderProjectsOverview.getHasRelatedPathfinderProjects(),
        null,
        Collections.singletonList(pathfinderProject.getPathfinderProjectId())
    );
  }

  @Test
  void savePathfinderProjectsForm_InvalidForm_AssertNeverSaves() throws Exception {
    var testForm = new PathfinderForm();

    doAnswer(invocation -> {
      var bindingResult = invocation.getArgument(1, BindingResult.class);
      bindingResult.rejectValue("hasPathfinderProjects", "mandatory", "validation message");
      return null;
    })
        .when(pathfinderFormValidator)
        .validate(eq(testForm), any(BindingResult.class));

    mockMvc.perform(post(ReverseRouter.route(on(PathfinderController.class).savePathfinderProjectsForm(SCAP_ID, null, null)))
            .with(authenticatedScapUser())
            .with(csrf())
            .flashAttr("form", testForm))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/pathfinder"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(SCAP_ID))))
        .andExpect(model().attribute("pathfinderSearchRestUrl",
            ReverseRouter.route(on(PathfinderRestController.class).getPathfinderSearchResults(SCAP_ID, null))))
        .andExpect(model().attribute("form", testForm));

    verifyNoInteractions(pathfinderService);
  }

  @Test
  void savePathfinderProjectsForm_ValidForm_AssertSaveAndRedirect() throws Exception {
    var testForm = new PathfinderForm();

    when(scapDetailService.getLatestByScapId(SCAP_ID)).thenReturn(scapDetail);
    when(pathfinderFormValidator.validate(testForm))
        .thenReturn(ValidatorTestingUtil.bindingResultWithoutErrors(testForm));

    mockMvc.perform(
        post(ReverseRouter.route(on(PathfinderController.class).savePathfinderProjectsForm(SCAP_ID, null, null)))
            .with(authenticatedScapUser())
            .with(csrf())
            .flashAttr("form", testForm))
        .andExpect(status().is3xxRedirection())
        .andExpect(redirectedUrl(ReverseRouter.route(on(TaskListController.class).renderTaskList(SCAP_ID))));

    verify(pathfinderService).saveRelatedPathfinderProjects(scapDetail, testForm);
  }
}
