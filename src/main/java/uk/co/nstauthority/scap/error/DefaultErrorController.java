package uk.co.nstauthority.scap.error;

import static javax.servlet.http.HttpServletResponse.SC_FORBIDDEN;
import static javax.servlet.http.HttpServletResponse.SC_METHOD_NOT_ALLOWED;
import static javax.servlet.http.HttpServletResponse.SC_NOT_FOUND;
import static javax.servlet.http.HttpServletResponse.SC_UNAUTHORIZED;

import java.util.Optional;
import javax.servlet.RequestDispatcher;
import javax.servlet.http.HttpServletRequest;
import org.apache.commons.lang.ObjectUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.web.servlet.error.ErrorController;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.servlet.DispatcherServlet;
import org.springframework.web.servlet.ModelAndView;
import org.springframework.web.util.WebUtils;

@Controller
public class DefaultErrorController implements ErrorController {

  private final ErrorService errorService;

  @Autowired
  public DefaultErrorController(ErrorService errorService) {
    this.errorService = errorService;
  }

  /**
   * Handles framework-level errors (404s, authorisation failures, filter exceptions) for browser clients. Errors thrown
   * by app code (controller methods and below) are handled in DefaultExceptionResolver.
   */
  @GetMapping("/error")
  public ModelAndView handleError(HttpServletRequest request) {
    var statusCode = Optional.ofNullable(request.getAttribute(RequestDispatcher.ERROR_STATUS_CODE))
        .map(Integer.class::cast);

    var viewName = statusCode.map(this::getViewName).orElse(ErrorView.DEFAULT_ERROR.getViewName());
    var modelAndView = new ModelAndView(viewName);

    var dispatcherException = request.getAttribute(DispatcherServlet.EXCEPTION_ATTRIBUTE);
    var servletException = request.getAttribute(WebUtils.ERROR_EXCEPTION_ATTRIBUTE);
    var throwable = (Throwable) ObjectUtils.defaultIfNull(dispatcherException, servletException);

    errorService.addErrorAttributesToModel(modelAndView, throwable);
    return modelAndView;
  }

  private String getViewName(int statusCode) {
    return switch (statusCode) {
      case SC_NOT_FOUND, SC_METHOD_NOT_ALLOWED -> ErrorView.PAGE_NOT_FOUND.getViewName();
      case SC_FORBIDDEN, SC_UNAUTHORIZED -> ErrorView.UNAUTHORISED.getViewName();
      default -> ErrorView.DEFAULT_ERROR.getViewName();
    };
  }

}
