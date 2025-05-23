package uk.co.nstauthority.scap.configuration;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.web.access.AccessDeniedHandler;

public class ServiceDeniedHandler implements AccessDeniedHandler {


  public ServiceDeniedHandler() {
  }

  @Override
  public void handle(HttpServletRequest request, HttpServletResponse response,
                     AccessDeniedException accessDeniedException) throws IOException, ServletException {
    response.sendRedirect(request.getServletContext().getContextPath() + "/error/forbidden");
  }
}
