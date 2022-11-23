package uk.co.nstauthority.scap.authentication;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrowsExactly;

import java.util.Set;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.context.SecurityContextImpl;

class UserDetailServiceTest {

  private static UserDetailService userDetailService;

  @BeforeAll
  static void setUp() {
    userDetailService = new UserDetailService();
  }

  @Test
  void getUserDetail_whenUserInContext_thenExpectedUserReturned() {

    var user = ServiceUserDetailTestUtil.Builder()
        .withWuaId(100L)
        .build();

    SamlAuthenticationUtil.Builder()
        .withUser(user)
        .setSecurityContext();

    assertThat(userDetailService.getUserDetail())
        .extracting(ServiceUserDetail::wuaId)
        .isEqualTo(user.wuaId());
  }

  @Test
  void getUserDetail_whenNoPrincipal_thenException() {

    SecurityContextHolder.setContext(new SecurityContextImpl(new ServiceSaml2Authentication(null, Set.of())));

    assertThrowsExactly(InvalidAuthenticationException.class, () -> userDetailService.getUserDetail(),
        "ServiceUserDetails not found in ServiceSaml2Authentication principal");
  }

  @Test
  void getUserDetail_whenNoAuthenticationInContext_thenException() {
    assertThrowsExactly(InvalidAuthenticationException.class, () -> userDetailService.getUserDetail(),
        "ServiceSaml2Authentication not found in authentication context");
  }
}