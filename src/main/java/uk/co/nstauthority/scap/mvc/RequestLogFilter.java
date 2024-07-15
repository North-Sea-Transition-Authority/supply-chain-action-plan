package uk.co.nstauthority.scap.mvc;

import com.google.common.base.Stopwatch;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.concurrent.TimeUnit;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;
import org.springframework.web.servlet.HandlerMapping;
import uk.co.nstauthority.scap.authentication.EnergyPortalSamlAttribute;
import uk.co.nstauthority.scap.correlationidutil.CorrelationIdUtil;
import uk.co.nstauthority.scap.jooq.JooqStatisticsListener;
import uk.co.nstauthority.scap.jpa.HibernateQueryCounterImpl;

@Component
public class RequestLogFilter extends OncePerRequestFilter {

  static final String MDC_WUA_ID = RequestLogFilter.class.getName() + ".%s".formatted(
      EnergyPortalSamlAttribute.WEB_USER_ACCOUNT_ID.getAttributeName()
  );
  static final String MDC_REQUEST_TYPE = RequestLogFilter.class.getName() + ".REQUEST_TYPE";
  private static final Logger LOGGER = LoggerFactory.getLogger(RequestLogFilter.class);
  private static final String UNKNOWN = "unknown";

  private final HibernateQueryCounterImpl hibernateQueryCounter;
  private final JooqStatisticsListener jooqStatisticsListener;

  @Autowired
  public RequestLogFilter(HibernateQueryCounterImpl hibernateQueryCounter,
                          JooqStatisticsListener jooqStatisticsListener) {
    this.hibernateQueryCounter = hibernateQueryCounter;
    this.jooqStatisticsListener = jooqStatisticsListener;
  }

  @Override
  protected void doFilterInternal(HttpServletRequest request,
                                  HttpServletResponse response,
                                  FilterChain filterChain) throws ServletException, IOException {

    var stopwatch = Stopwatch.createStarted();
    hibernateQueryCounter.clearQueryCount();
    String correlationId = "";

    try {
      correlationId = CorrelationIdUtil.getOrCreateCorrelationId(request);
      CorrelationIdUtil.setCorrelationIdOnMdc(correlationId);
      filterChain.doFilter(request, response);
    } finally {
      String queryString = StringUtils.defaultString(request.getQueryString());

      if (!queryString.isEmpty()) {
        queryString = "?" + queryString;
      }

      Long hibernateQueryCount = hibernateQueryCounter.getQueryCount();

      Object patternAttribute = request.getAttribute(HandlerMapping.BEST_MATCHING_PATTERN_ATTRIBUTE);
      String mvcPattern = StringUtils.firstNonBlank(patternAttribute != null ? patternAttribute.toString() : "", UNKNOWN);

      String requestType = StringUtils.firstNonBlank(MDC.get(MDC_REQUEST_TYPE), UNKNOWN);
      String userId = StringUtils.firstNonBlank(MDC.get(MDC_WUA_ID), UNKNOWN);

      var jooqQueryCount = jooqStatisticsListener.getCount();
      jooqStatisticsListener.clear();
      CorrelationIdUtil.clearCorrelationIdOnMdc();

      LOGGER.info(
          "{} request: {} {}{} ({}), correlation id: {}, time: {}, status: {}, user id: {}, " +
              "hibernate query count: {}, jooq query count: {}",
          requestType, request.getMethod(), request.getRequestURI(), queryString,
          mvcPattern, correlationId, stopwatch.elapsed(TimeUnit.MILLISECONDS),
          response.getStatus(), userId, hibernateQueryCount, jooqQueryCount);
    }
  }
}
