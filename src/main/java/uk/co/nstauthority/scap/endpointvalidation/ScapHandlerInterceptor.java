package uk.co.nstauthority.scap.endpointvalidation;

import static uk.co.nstauthority.scap.mvc.AbstractHandlerInterceptor.getPathVariableByClass;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.validation.constraints.NotNull;
import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.web.method.HandlerMethod;
import org.springframework.web.servlet.HandlerInterceptor;
import org.springframework.web.servlet.HandlerMapping;
import uk.co.nstauthority.scap.authentication.UserDetailService;
import uk.co.nstauthority.scap.permissionmanagement.Team;
import uk.co.nstauthority.scap.permissionmanagement.TeamId;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.util.HandlerInterceptorUtil;

@Component
public class ScapHandlerInterceptor implements HandlerInterceptor {

  private static final Logger LOGGER = LoggerFactory.getLogger(ScapHandlerInterceptor.class);

  private final ScapDetailService scapDetailService;

  private final TeamService teamService;

  private final UserDetailService userDetailService;
  private final List<ScapSecurityRule> securityRules;

  @Autowired
  ScapHandlerInterceptor(ScapDetailService scapDetailService,
                         TeamService teamService,
                         UserDetailService userDetailService,
                         List<ScapSecurityRule> securityRules) {
    this.scapDetailService = scapDetailService;
    this.teamService = teamService;
    this.userDetailService = userDetailService;
    this.securityRules = securityRules;
  }

  @Override
  public boolean preHandle(@NotNull HttpServletRequest request,
                           @NotNull HttpServletResponse response,
                           @NotNull Object handler) throws IOException {
    if (!(handler instanceof HandlerMethod)) {
      LOGGER.warn("No handler annotations detected for this endpoint");
      return true;
    }
    var handlerMethod = (HandlerMethod) handler;

    // Check if any ScapSecurityRule-based annotations are applied to the requested endpoint.
    // This avoids having to apply an additional annotation to every endpoint we want to have security
    var hasSecurityAnnotation = securityRules.stream()
        .anyMatch(securityRule ->
            Objects.nonNull(HandlerInterceptorUtil.findMethodOrClassAnnotation(securityRule.supports(), handlerMethod)));
    if (!hasSecurityAnnotation) {
      return true;
    }

    var scapDetail = extractScapDetailFromRequest(request, handlerMethod);
    var team = extractTeamFromRequest(request, handlerMethod);
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
          scapDetail,
          team
      );

      var hasRulePassed = HandlerInterceptorUtil.processRedirectsAndReturnResult(result, response);
      if (!hasRulePassed) {
        return false;
      }
    }
    return true;
  }

  private ScapDetail extractScapDetailFromRequest(HttpServletRequest httpServletRequest, HandlerMethod handlerMethod) {
    var scapIdParameter = getPathVariableByClass(handlerMethod, ScapId.class);
    if (scapIdParameter.isEmpty()) {
      return null;
    }

    @SuppressWarnings("unchecked")
    var pathVariables = (Map<String, String>) httpServletRequest
        .getAttribute(HandlerMapping.URI_TEMPLATE_VARIABLES_ATTRIBUTE);

    return scapDetailService.getActionableScapDetail(
        ScapId.valueOf(pathVariables.get(scapIdParameter.get().getName())),
        userDetailService.getUserDetail());
  }

  public Team extractTeamFromRequest(HttpServletRequest httpServletRequest, HandlerMethod handlerMethod) {
    var teamIdParameter = getPathVariableByClass(handlerMethod, TeamId.class);
    if (teamIdParameter.isEmpty()) {
      return null;
    }

    @SuppressWarnings("unchecked")
    var pathVariables = (Map<String, String>) httpServletRequest
        .getAttribute(HandlerMapping.URI_TEMPLATE_VARIABLES_ATTRIBUTE);

    return teamService.getTeam(TeamId.valueOf(pathVariables.get(teamIdParameter.get().getName())));
  }
}
