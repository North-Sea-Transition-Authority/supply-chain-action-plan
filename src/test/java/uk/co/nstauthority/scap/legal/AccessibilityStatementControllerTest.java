package uk.co.nstauthority.scap.legal;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
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

@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes = AccessibilityStatementController.class)
@WithMockUser
class AccessibilityStatementControllerTest extends AbstractControllerTest {

  @Test
  void renderAccessibilityStatement() throws Exception {
    mockMvc.perform(get(
        ReverseRouter.route(on(AccessibilityStatementController.class).renderAccessibilityStatement()))
            .with(authenticatedScapUser()))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/legal/accessibilityStatement"));
  }

}