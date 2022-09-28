package uk.co.nstauthority.scap.workarea;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.model;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.view;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import uk.co.nstauthority.scap.AbstractControllerTest;
import uk.co.nstauthority.scap.application.start.ScapStartController;
import uk.co.nstauthority.scap.mvc.ReverseRouter;

@ExtendWith(MockitoExtension.class)
@WebMvcTest(controllers = WorkAreaController.class)
public class WorkAreaControllerTest extends AbstractControllerTest {

  @Test
  public void getWorkArea() throws Exception{

    mockMvc.perform(
        get(ReverseRouter.route(on(WorkAreaController.class).getWorkArea())))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/workarea/workArea"))
        .andExpect(model().attribute("startScapUrl",
            ReverseRouter.route(on(ScapStartController.class).renderStartNewScap())));

  }

}
