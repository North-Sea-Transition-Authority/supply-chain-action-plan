package uk.co.nstauthority.scap.endpointvalidation.rules;


import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.lang.annotation.Annotation;
import java.util.Arrays;
import java.util.Optional;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;
import uk.co.nstauthority.scap.authentication.ServiceUserDetail;
import uk.co.nstauthority.scap.endpointvalidation.ScapSecurityRule;
import uk.co.nstauthority.scap.endpointvalidation.SecurityRuleResult;
import uk.co.nstauthority.scap.endpointvalidation.annotations.ScapHasStatus;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.Team;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.summary.ScapSummaryController;

@Component
public class ScapHasStatusRule implements ScapSecurityRule {

  private final ScapDetailService scapDetailService;

  private static final Logger LOGGER = LoggerFactory.getLogger(ScapHasStatusRule.class);

  @Autowired
  public ScapHasStatusRule(ScapDetailService scapDetailService) {
    this.scapDetailService = scapDetailService;
  }

  @Override
  public Class<? extends Annotation> supports() {
    return ScapHasStatus.class;
  }

  @Override
  public SecurityRuleResult check(Object annotation,
                                  HttpServletRequest request,
                                  HttpServletResponse response,
                                  ServiceUserDetail userDetail,
                                  ScapDetail scapDetail,
                                  Team team) {

    var permittedStatuses = ((ScapHasStatus) annotation).permittedStatuses();
    var scapStatus = scapDetail.getStatus();

    var inPermittedStatus = Arrays.asList(permittedStatuses).contains(scapStatus);

    if (inPermittedStatus) {
      return SecurityRuleResult.continueAsNormal();
    }

    LOGGER.error("Cannot perform this action on a SCAP with status: %s".formatted(scapDetail.getStatus()));
    if (ScapDetailStatus.SUBMITTED.equals(scapStatus)) {
      var scapId = scapDetail.getScap().getScapId();
      var loggerMsg = "SCAP with ID [%s] has status %s. Redirecting user with ID [%d] to SCAP summary page.".formatted(
          scapDetail.getScap().getScapId(),
          scapStatus.getEnumName(),
          userDetail.wuaId()
      );
      LOGGER.info(loggerMsg);

      var contextPath = Optional.ofNullable(request.getContextPath()).orElse("");
      var summaryEndpoint = contextPath.concat(
          ReverseRouter.route(on(ScapSummaryController.class).getScapSummary(scapId)
          ));
      return SecurityRuleResult.cancelRemainingHandlerInterceptorsAndRedirect(summaryEndpoint);
    }
    return SecurityRuleResult.checkFailedWithStatus(HttpStatus.BAD_REQUEST);
  }
}
