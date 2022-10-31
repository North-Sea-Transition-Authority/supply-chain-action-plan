package uk.co.nstauthority.scap.energyportal;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatNoException;

import java.util.Arrays;
import java.util.List;
import java.util.UUID;
import org.junit.jupiter.api.Test;
import uk.co.nstauthority.scap.branding.CustomerConfigurationProperties;
import uk.co.nstauthority.scap.branding.ServiceBrandingConfigurationProperties;

class EnergyPortalApiWrapperTest {

  private final ServiceBrandingConfigurationProperties serviceBrandingConfigurationProperties = new ServiceBrandingConfigurationProperties(
      new CustomerConfigurationProperties("name", "mnemonic", "guidanceDocumentUrl"),
      null
  );

  private final EnergyPortalApiWrapper energyPortalApiWrapper = new EnergyPortalApiWrapper(
      serviceBrandingConfigurationProperties
  );

  @Test
  void makeRequest_verifyLogCorrelationId() {

    var logCorrelationId = energyPortalApiWrapper.makeRequest(this::returnLogCorrelationId);

    List<String> logCorrelationIdComponents = Arrays.asList(logCorrelationId.split(" "));

    assertThat(logCorrelationIdComponents).hasSize(3);
    assertThat(logCorrelationIdComponents.get(0)).isEqualTo(serviceBrandingConfigurationProperties.getCustomerConfigurationProperties().mnemonic());
    assertThat(logCorrelationIdComponents.get(1)).isEqualTo(EnergyPortalApiWrapper.API_REQUEST_PREFIX + ":");
    assertThatNoException().isThrownBy(() -> UUID.fromString(logCorrelationIdComponents.get(2)));
  }

  @Test
  void makeRequest_verifyRequestPurpose() {

    var requestPurpose = energyPortalApiWrapper.makeRequest(this::returnRequestPurpose);

    assertThat(requestPurpose).isEqualTo("%s: %s.%s".formatted(
        serviceBrandingConfigurationProperties.getCustomerConfigurationProperties().mnemonic(),
        this.getClass().getName(),
        "makeRequest_verifyRequestPurpose"
    ));
  }

  private String returnLogCorrelationId(LogCorrelationId logCorrelationId, RequestPurpose requestPurpose) {
    return logCorrelationId.id();
  }

  private String returnRequestPurpose(LogCorrelationId logCorrelationId, RequestPurpose requestPurpose) {
    return requestPurpose.purpose();
  }
}
