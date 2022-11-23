package uk.co.nstauthority.scap.authentication;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;
import static org.mockito.Mockito.doCallRealMethod;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;
import static uk.co.nstauthority.scap.authentication.TestUserProvider.user;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import uk.co.nstauthority.scap.AbstractControllerTest;
import uk.co.nstauthority.scap.mvc.ReverseRouter;

@ContextConfiguration(classes = {
    TestAuthenticationController.class,
    UserDetailService.class
})
class TestAuthenticationControllerTest extends AbstractControllerTest {

  @Autowired
  UserDetailService userDetailService;

  // needs to be BeforeEach as otherwise MockBean in AbstractControllerTest is not initialised yet
  @BeforeEach
  void setUp() {
    doCallRealMethod().when(userDetailService).getUserDetail();
  }

  @Test
  void whenNoUser_thenVerifyAuthenticationRequired() throws Exception {
    mockMvc.perform(
        get(ReverseRouter.route(on(TestAuthenticationController.class).requiresUserEndpoint())))
        .andExpect(status().isUnauthorized());
  }

  @Test
  void whenUser_thenVerifyAuthorised() throws Exception {

    var expectedUser = ServiceUserDetailTestUtil.Builder()
        .withWuaId(100L)
        .build();

    var modelAndView = mockMvc.perform(
            get(ReverseRouter.route(on(TestAuthenticationController.class).requiresUserEndpoint()))
                .with(user(expectedUser))
        )
        .andExpect(status().isOk())
        .andReturn()
        .getModelAndView();

    assertThat(modelAndView).isNotNull();

    assertThat((ServiceUserDetail) modelAndView.getModel().get("user"))
        .extracting(ServiceUserDetail::wuaId)
        .isEqualTo(expectedUser.wuaId());
  }

}