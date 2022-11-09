package uk.co.nstauthority.scap.scap.start;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.model;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.view;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ContextConfiguration;
import uk.co.nstauthority.scap.AbstractControllerTest;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.organisationgroup.OrganisationGroupController;
import uk.co.nstauthority.scap.workarea.WorkAreaController;

@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes = ScapStartController.class)
@WithMockUser
class ScapStartControllerTest extends AbstractControllerTest {

  @Test
  void renderStartNewScap() throws Exception {
    mockMvc.perform(get(
        ReverseRouter.route(on(ScapStartController.class).renderStartNewScap())))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/start"))
        .andExpect(model().attribute("startScapRedirectUrl",
            ReverseRouter.route(on(OrganisationGroupController.class).renderNewScapOrganisationGroupForm(null))))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(WorkAreaController.class).getWorkArea())));
  }
}
