package uk.co.nstauthority.scap.mvc;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.validation.constraints.NotNull;
import java.io.IOException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;
import uk.co.nstauthority.scap.authentication.UserDetailService;

@Component
public class PostAuthenticationRequestMdcFilter extends OncePerRequestFilter {

  private final UserDetailService userDetailService;
  private static final Logger LOGGER = LoggerFactory.getLogger(PostAuthenticationRequestMdcFilter.class);

  @Autowired
  PostAuthenticationRequestMdcFilter(UserDetailService userDetailService) {
    this.userDetailService = userDetailService;
  }

  @Override
  protected void doFilterInternal(@NotNull HttpServletRequest request,
                                  @NotNull HttpServletResponse response,
                                  @NotNull FilterChain filterChain) throws ServletException, IOException {

    try {
      if (userDetailService.isUserLoggedIn()) {
        var authenticatedUser = userDetailService.getUserDetail();
        MDC.put(RequestLogFilter.MDC_WUA_ID, authenticatedUser.wuaId().toString());
        MDC.put(RequestLogFilter.MDC_REQUEST_TYPE, "authenticated");
      } else {
        MDC.put(RequestLogFilter.MDC_REQUEST_TYPE, "guest");
      }
    } catch (Exception e) {
      LOGGER.error("Error getting user details for MDC", e);
    }

    filterChain.doFilter(request, response);
  }
}
