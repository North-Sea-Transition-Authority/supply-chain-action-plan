package uk.co.nstauthority.scap.mvc;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.validation.constraints.NotNull;
import org.springframework.stereotype.Service;
import org.springframework.web.servlet.HandlerInterceptor;

/**
 * If the output buffer is too small, any errors during template rendering may occur after the buffer has been flushed and
 * html sent to the client. This results in a 200 response and the error page being included embedded in the page at the point
 * of the error.
 * Setting a large buffer size ensures that the entire page will be rendered (and checked for errors) before sending a 200
 * back to the client.
 */
@Service
public class ResponseBufferSizeHandlerInterceptor implements HandlerInterceptor {

  @Override
  public boolean preHandle(@NotNull HttpServletRequest request, HttpServletResponse response, @NotNull Object handler) {
    response.setBufferSize(1024 * 200);
    return true;
  }
}
