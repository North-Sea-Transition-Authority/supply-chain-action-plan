package uk.co.nstauthority.scap.util;

import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.lang.annotation.Annotation;
import java.util.Objects;
import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.web.method.HandlerMethod;
import org.springframework.web.server.ResponseStatusException;
import uk.co.nstauthority.scap.endpointvalidation.SecurityRuleResult;

public class HandlerInterceptorUtil {

  private HandlerInterceptorUtil() {
    throw new IllegalStateException("This is a helper class, it should not be instantiated");
  }

  public static <A extends Annotation> A findMethodOrClassAnnotation(Class<A> annotationClass,
                                                                     HandlerMethod handlerMethod) {
    var method = handlerMethod.getMethod();
    // Check if the method has the desired annotation
    var methodAnnotation = AnnotationUtils.findAnnotation(method, annotationClass);
    if (methodAnnotation != null) {
      return methodAnnotation;
    }

    // Fallback and check if the class contains the annotation
    return AnnotationUtils.findAnnotation(method.getDeclaringClass(), annotationClass);
  }

  public static boolean processRedirectsAndReturnResult(SecurityRuleResult securityRuleResult,
                                                        HttpServletResponse response) throws IOException {
    if (!securityRuleResult.hasRulePassed()) {
      var redirectUrl = securityRuleResult.redirectUrl();
      if (Objects.nonNull(redirectUrl)) {
        response.sendRedirect(redirectUrl);
      }
      var failureStatus = securityRuleResult.failureStatus();
      if (Objects.nonNull(failureStatus)) {
        throw new ResponseStatusException(failureStatus);
      }
      return false;
    }
    return true;
  }
}
