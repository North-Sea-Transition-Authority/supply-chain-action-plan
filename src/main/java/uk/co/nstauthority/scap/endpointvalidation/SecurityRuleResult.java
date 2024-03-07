package uk.co.nstauthority.scap.endpointvalidation;

import org.springframework.http.HttpStatus;

public record SecurityRuleResult(
    boolean hasRulePassed,
    HttpStatus failureStatus,
    String redirectUrl
) {
  /**
   * Indicates that the security rule has passed all its checks and the process flow should continue as normal.
   *
   * @return security rule result that doesn't alter the handler interceptors normal operating behaviour
   */
  public static SecurityRuleResult continueAsNormal() {
    return new SecurityRuleResult(true, null, null);
  }

  public static SecurityRuleResult checkFailedWithStatus(HttpStatus httpStatus) {
    return new SecurityRuleResult(false, httpStatus, null);
  }

  /**
   * Indicates that the handler interceptor should not execute Spring's remaining handler interceptors. This security
   * rule dealt with the response itself and no further Spring intervention is required.
   *
   * @return security rule result that redirects the response and cancels all Spring handler interceptors
   */
  public static SecurityRuleResult cancelRemainingHandlerInterceptorsAndRedirect(String redirectUrl) {
    return new SecurityRuleResult(false, null, redirectUrl);
  }
}
