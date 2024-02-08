package uk.co.nstauthority.scap.authentication;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrowsExactly;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Collection;
import java.util.Set;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.springframework.security.authentication.AbstractAuthenticationToken;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.context.SecurityContextImpl;
import uk.co.nstauthority.scap.error.exception.InvalidAuthenticationException;

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

  @Test
  void isUserLoggedIn_whenUserInContext_thenTrue() {
    var user = ServiceUserDetailTestUtil.Builder()
        .withWuaId(100L)
        .build();

    SamlAuthenticationUtil.Builder()
        .withUser(user)
        .setSecurityContext();

    assertTrue(userDetailService.isUserLoggedIn());
  }

  @Test
  void isUserLoggedIn_whenNoPrincipal_thenException() {
    assertFalse(userDetailService.isUserLoggedIn());
  }

  @Test
  void isUserLoggedIn_whenNoAuthenticationInContext_thenException() {
    SecurityContextHolder.setContext(new SecurityContextImpl(new ServiceSaml2Authentication(null, Set.of())));
    assertFalse(userDetailService.isUserLoggedIn());
  }

  @Test
  void isUserLoggedIn_whenUserInContext_thenFalse() {
    SecurityContextHolder.getContext().setAuthentication(new NonValidAuthentication(null));

    assertFalse(userDetailService.isUserLoggedIn());
  }

  private class NonValidAuthentication extends AbstractAuthenticationToken {

    NonValidAuthentication(Collection<? extends GrantedAuthority> authorities) {
      super(authorities);
    }

    @Override
    public Object getCredentials() {
      return null;
    }

    @Override
    public Object getPrincipal() {
      return false;
    }
  }
}