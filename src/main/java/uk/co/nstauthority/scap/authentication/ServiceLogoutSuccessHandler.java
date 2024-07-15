package uk.co.nstauthority.scap.authentication;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.Authentication;
import org.springframework.security.web.authentication.logout.LogoutSuccessHandler;
import org.springframework.stereotype.Component;
import uk.co.nstauthority.scap.configuration.SamlProperties;

@Component
public class ServiceLogoutSuccessHandler implements LogoutSuccessHandler {
  private final SamlProperties samlProperties;

  @Autowired
  public ServiceLogoutSuccessHandler(SamlProperties samlProperties) {
    this.samlProperties = samlProperties;
  }

  @Override
  public void onLogoutSuccess(HttpServletRequest request,
                              HttpServletResponse response,
                              Authentication authentication) throws IOException {
    response.sendRedirect(samlProperties.getLogoutUrl());
  }
}
