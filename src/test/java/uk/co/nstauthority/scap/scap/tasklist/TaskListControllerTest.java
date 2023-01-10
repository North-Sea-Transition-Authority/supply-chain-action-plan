package uk.co.nstauthority.scap.scap.tasklist;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.model;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.view;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.Collections;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ContextConfiguration;
import uk.co.nstauthority.scap.AbstractControllerTest;
import uk.co.nstauthority.scap.AbstractScapSubmitterControllerTest;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.scap.ScapId;

@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes = TaskListController.class)
@WithMockUser
class TaskListControllerTest extends AbstractScapSubmitterControllerTest {

  @MockBean
  private List<ScapTaskListSection> scapTaskListSections;

  @MockBean
  private List<ScapTaskListItem> scapTaskListItems;

  @Test
  void renderTaskList() throws Exception {
    mockMvc.perform(
        get(ReverseRouter.route(on(TaskListController.class).renderTaskList(SCAP_ID))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/taskList"))
        .andExpect(model().attribute("backLinkUrl", TaskListController.WORK_AREA_URL))
        .andExpect(model().attribute("taskListSections", Collections.emptyList()));
  }

}
