package uk.co.nstauthority.scap.authentication;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.web.context.WebApplicationContext;
import uk.co.nstauthority.scap.AbstractControllerTest;
import uk.co.nstauthority.scap.configuration.SamlProperties;
import uk.co.nstauthority.scap.mvc.ReverseRouter;

@ContextConfiguration(classes = LogoutController.class)
class LogoutControllerTest extends AbstractControllerTest {

  private static final Class<LogoutController> CONTROLLER = LogoutController.class;
  private static final String UNAUTHORIZED_KEY = "Bearer INVALID_KEY";
  private static final Long WUA_ID = 1L;

  @Autowired
  private SamlProperties samlProperties;

  @Autowired
  protected WebApplicationContext context;

  @MockBean
  private LogoutService logoutService;

  @Test
  void logoutService() throws Exception {
    mockMvc
        .perform(post(ReverseRouter.route(on(CONTROLLER).logoutOfService(null, WUA_ID)))
            .header("Authorization", "Bearer " + samlProperties.getLogoutKey()))
        .andExpect(status().isOk());
    verify(logoutService).logoutUser(WUA_ID);
  }

  @Test
  void logoutService_unauthorized() throws Exception {
    mockMvc
        .perform(post(ReverseRouter.route(on(CONTROLLER).logoutOfService(null, WUA_ID)))
            .header("Authorization", UNAUTHORIZED_KEY))
        .andExpect(status().isForbidden());
    verify(logoutService, never()).logoutUser(any());
  }

  @ParameterizedTest
  @ValueSource(strings = {"INVALID_KEY", "test"})
  void logoutService_invalidKey(String preSharedKey) throws Exception {
    mockMvc
        .perform(post(ReverseRouter.route(on(CONTROLLER).logoutOfService(null, WUA_ID)))
            .header("Authorization", preSharedKey))
        .andExpect(status().isForbidden());
    verify(logoutService, never()).logoutUser(any());
  }
}