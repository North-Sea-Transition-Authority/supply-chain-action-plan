package uk.co.nstauthority.scap.endpointvalidation;

import static uk.co.nstauthority.scap.mvc.AbstractHandlerInterceptor.getPathVariableByClass;

import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.jetbrains.annotations.NotNull;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.web.method.HandlerMethod;
import org.springframework.web.servlet.HandlerInterceptor;
import org.springframework.web.servlet.HandlerMapping;
import uk.co.nstauthority.scap.authentication.UserDetailService;
import uk.co.nstauthority.scap.scap.scap.Scap;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.scap.ScapService;
import uk.co.nstauthority.scap.util.HandlerInterceptorUtil;

@Component
public class ScapHandlerInterceptor implements HandlerInterceptor {

  private final ScapService scapService;

  private final UserDetailService userDetailService;
  private final List<ScapSecurityRule> securityRules;

  @Autowired
  ScapHandlerInterceptor(ScapService scapService,
                         UserDetailService userDetailService,
                         List<ScapSecurityRule> securityRules) {
    this.scapService = scapService;
    this.userDetailService = userDetailService;
    this.securityRules = securityRules;
  }

  @Override
  public boolean preHandle(@NotNull HttpServletRequest request,
                           @NotNull HttpServletResponse response,
                           @NotNull Object handler) throws IOException {
    var handlerMethod = (HandlerMethod) handler;

    // Check if any ScapSecurityRule-based annotations are applied to the requested endpoint.
    // This avoids having to apply an additional annotation to every endpoint we want to have security
    var hasSecurityAnnotation = securityRules.stream()
        .anyMatch(securityRule ->
            Objects.nonNull(HandlerInterceptorUtil.findMethodOrClassAnnotation(securityRule.supports(), handlerMethod)));
    if (!hasSecurityAnnotation) {
      return true;
    }

    var scap = extractScapFromRequest(request, handlerMethod);
    var userDetail = userDetailService.getUserDetail();

    for (var securityRule : securityRules) {
      var annotationObject = HandlerInterceptorUtil.findMethodOrClassAnnotation(securityRule.supports(), handlerMethod);
      if (Objects.isNull(annotationObject)) {
        continue;
      }
      var result = securityRule.check(
          annotationObject,
          request,
          response,
          userDetail,
          scap
      );

      var hasRulePassed = HandlerInterceptorUtil.processRedirectsAndReturnResult(result, response);
      if (!hasRulePassed) {
        return false;
      }
    }
    return true;
  }

  private Scap extractScapFromRequest(HttpServletRequest httpServletRequest, HandlerMethod handlerMethod) {

    var scapIdParameter = getPathVariableByClass(handlerMethod, ScapId.class);

    if (scapIdParameter.isEmpty()) {
      return null;
    }

    @SuppressWarnings("unchecked")
    var pathVariables = (Map<String, String>) httpServletRequest
        .getAttribute(HandlerMapping.URI_TEMPLATE_VARIABLES_ATTRIBUTE);

    return scapService.getScapById(Integer.valueOf(pathVariables.get(scapIdParameter.get().getName())));
  }
}
