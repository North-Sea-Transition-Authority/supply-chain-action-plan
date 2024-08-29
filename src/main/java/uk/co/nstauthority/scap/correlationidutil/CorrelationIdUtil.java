package uk.co.nstauthority.scap.correlationidutil;

import java.util.UUID;
import javax.servlet.http.HttpServletRequest;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;

public class CorrelationIdUtil {

  public static final String HTTP_CORRELATION_ID_HEADER = "energy-portal-correlation-id";
  public static final String MDC_CORRELATION_ID_ATTR = "CORRELATION_ID";
  private static final Logger LOGGER = LoggerFactory.getLogger(CorrelationIdUtil.class);

  CorrelationIdUtil() {
    throw new IllegalStateException("Cannot instantiate static helper");
  }

  static boolean isCorrelationIdSetOnMdc() {
    var existingCorrelationId = MDC.get(MDC_CORRELATION_ID_ATTR);
    return StringUtils.isNotBlank(existingCorrelationId);
  }

  public static String getCorrelationIdFromMdc() {
    return MDC.get(MDC_CORRELATION_ID_ATTR);
  }

  public static void setCorrelationIdOnMdc(String value) {
    if (isCorrelationIdSetOnMdc()) {
      var existingCorrelationId = MDC.get(MDC_CORRELATION_ID_ATTR);
      LOGGER.warn("Overwriting existing correlationId - {}", existingCorrelationId);
    }

    MDC.put(MDC_CORRELATION_ID_ATTR, value);
  }

  public static void clearCorrelationIdOnMdc() {
    MDC.remove(MDC_CORRELATION_ID_ATTR);
  }

  public static String getOrCreateCorrelationId(HttpServletRequest request) {
    var existingCorrelationId = request.getHeader(HTTP_CORRELATION_ID_HEADER);
    if (StringUtils.isBlank(existingCorrelationId)) {
      return UUID.randomUUID().toString();
    } else {
      LOGGER.debug("Accepted correlationId from request - {}", existingCorrelationId);
      return existingCorrelationId;
    }
  }
}
