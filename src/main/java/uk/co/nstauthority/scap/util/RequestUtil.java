package uk.co.nstauthority.scap.util;

import jakarta.servlet.http.HttpServletRequest;
import java.util.Map;
import java.util.Optional;
import org.springframework.web.servlet.HandlerMapping;
import uk.co.nstauthority.scap.scap.scap.ScapId;

public class RequestUtil {

  private static final String SCAP_ID_ATTR = "scapId";

  private RequestUtil() {
  }

  @SuppressWarnings("unchecked")
  public static Optional<ScapId> getApplicationId(HttpServletRequest request) {
    return Optional.ofNullable(request.getAttribute(HandlerMapping.URI_TEMPLATE_VARIABLES_ATTRIBUTE))
        .map(o -> (Map<String, String>) o)
        .map(map -> map.get(SCAP_ID_ATTR))
        .map(ScapId::valueOf);
  }
}
