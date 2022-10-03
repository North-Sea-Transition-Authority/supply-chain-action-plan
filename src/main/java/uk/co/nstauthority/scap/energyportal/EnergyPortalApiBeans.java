package uk.co.nstauthority.scap.energyportal;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import uk.co.fivium.energyportalapi.client.EnergyPortal;
import uk.co.fivium.energyportalapi.client.organisation.OrganisationApi;

@Configuration
public class EnergyPortalApiBeans {

  @Bean
  EnergyPortal energyPortal(EnergyPortalApiConfig energyPortalApiConfig) {
    if (!energyPortalApiConfig.sslVerification()) {
      return EnergyPortal.defaultConfigurationWithoutSslVerification(
          energyPortalApiConfig.url(), energyPortalApiConfig.preSharedKey());
    }
    return EnergyPortal.defaultConfiguration(energyPortalApiConfig.url(), energyPortalApiConfig.preSharedKey());
  }

  @Bean
  public OrganisationApi organisationApi(EnergyPortal energyPortal) {
    return new OrganisationApi(energyPortal);
  }
}
