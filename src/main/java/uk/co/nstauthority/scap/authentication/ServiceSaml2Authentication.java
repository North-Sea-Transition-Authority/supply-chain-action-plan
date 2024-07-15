package uk.co.nstauthority.scap.authentication;

import java.util.Collection;
import org.springframework.security.authentication.AbstractAuthenticationToken;
import org.springframework.security.core.GrantedAuthority;

public class ServiceSaml2Authentication extends AbstractAuthenticationToken {

  private final ServiceUserDetail principal;

  public ServiceSaml2Authentication(ServiceUserDetail principal, Collection<? extends GrantedAuthority> authorities) {
    super(authorities);
    this.principal = principal;
    setAuthenticated(true);
  }

  @Override
  public Object getCredentials() {
    return null;
  }

  @Override
  public Object getPrincipal() {
    return principal;
  }

}
