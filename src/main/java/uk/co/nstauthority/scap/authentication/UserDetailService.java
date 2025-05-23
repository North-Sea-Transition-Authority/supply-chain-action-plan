package uk.co.nstauthority.scap.authentication;

import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.error.exception.InvalidAuthenticationException;


@Service
public class UserDetailService {

  public ServiceUserDetail getUserDetail() {
    if (SecurityContextHolder.getContext().getAuthentication() instanceof ServiceSaml2Authentication authentication) {
      if (authentication.getPrincipal() instanceof ServiceUserDetail serviceUserDetail) {
        return serviceUserDetail;
      } else {
        throw new InvalidAuthenticationException("ServiceUserDetails not found in ServiceSaml2Authentication principal");
      }
    } else {
      throw new InvalidAuthenticationException("ServiceSaml2Authentication not found in authentication context");
    }
  }

  public boolean isUserLoggedIn() {
    if (SecurityContextHolder.getContext().getAuthentication() instanceof ServiceSaml2Authentication authentication) {
      return authentication.getPrincipal() instanceof ServiceUserDetail;
    }
    return false;
  }

}
