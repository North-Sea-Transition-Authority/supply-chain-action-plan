package uk.co.nstauthority.scap.energyportal;

import java.util.UUID;
import java.util.function.BiFunction;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import uk.co.nstauthority.scap.branding.ServiceBrandingConfigurationProperties;

@Component
public class EnergyPortalApiWrapper {

  private static final Logger LOGGER = LoggerFactory.getLogger(EnergyPortalApiWrapper.class);

  static final String API_REQUEST_PREFIX = "EPA-request";

  private final ServiceBrandingConfigurationProperties serviceBrandingConfigurationProperties;

  @Autowired
  public EnergyPortalApiWrapper(ServiceBrandingConfigurationProperties serviceBrandingConfigurationProperties) {
    this.serviceBrandingConfigurationProperties = serviceBrandingConfigurationProperties;
  }

  public <T> T makeRequest(BiFunction<LogCorrelationId, RequestPurpose, T> request) {
    var logCorrelationId = getDefaultLogCorrelationId();
    var requestPurpose = getRequestPurpose();
    logEpaRequest(logCorrelationId, requestPurpose);
    return request.apply(logCorrelationId, requestPurpose);
  }

  private RequestPurpose getRequestPurpose() {

    var callingMethod = StackWalker.getInstance()
        .walk(frames -> frames
            .skip(2) // the first frame is this method, second is the BiFunction request so skip to get the real caller
            .findFirst()
            .map(stackFrame -> "%s.%s".formatted(stackFrame.getClassName(), stackFrame.getMethodName()))
        )
        .orElseThrow(() -> new RuntimeException("Failed to find a stack frame for request purpose"));

    return new RequestPurpose("%s: %s".formatted(getServiceIdentifier(), callingMethod));
  }

  private LogCorrelationId getLogCorrelationId(String logCorrelationId) {
    return new LogCorrelationId("%s %s: %s".formatted(getServiceIdentifier(), API_REQUEST_PREFIX, logCorrelationId));
  }

  private LogCorrelationId getDefaultLogCorrelationId() {
    return getLogCorrelationId(String.valueOf(UUID.randomUUID()));
  }

  private void logEpaRequest(LogCorrelationId logCorrelationId, RequestPurpose requestPurpose) {
    LOGGER.info("%s (%s)".formatted(logCorrelationId.id(), requestPurpose.purpose()));
  }

  private String getServiceIdentifier() {
    return serviceBrandingConfigurationProperties.getCustomerConfigurationProperties().mnemonic();
  }
}
