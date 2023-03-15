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
@ContextConfiguration(classes = ContactInformationController.class)
@WithMockUser
class ContactInformationControllerTest extends AbstractControllerTest {

  @Test
  void renderContactInformation() throws Exception {
    mockMvc.perform(get(ReverseRouter.route(on(ContactInformationController.class).renderContactInformation())))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/legal/contactInformation"));
  }
}
