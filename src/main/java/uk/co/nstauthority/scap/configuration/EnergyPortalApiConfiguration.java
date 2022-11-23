package uk.co.nstauthority.scap.configuration;

import javax.validation.constraints.NotNull;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.validation.annotation.Validated;
import uk.co.fivium.energyportalapi.client.EnergyPortal;
import uk.co.fivium.energyportalapi.client.countries.CountryApi;
import uk.co.fivium.energyportalapi.client.field.FieldApi;
import uk.co.fivium.energyportalapi.client.organisation.OrganisationApi;
import uk.co.fivium.energyportalapi.client.user.UserApi;

@Configuration
@ConfigurationProperties(prefix = "energy-portal-api")
@Validated
public class EnergyPortalApiConfiguration {

  @NotNull String url;
  @NotNull String preSharedKey;

  @Bean
  public EnergyPortal energyPortal() {
    return EnergyPortal.defaultConfiguration(url, preSharedKey);
  }

  @Bean
  public OrganisationApi organisationApi(EnergyPortal energyPortal) {
    return new OrganisationApi(energyPortal);
  }

  @Bean
  public CountryApi countryApi(EnergyPortal energyPortal) {
    return new CountryApi(energyPortal);
  }

  @Bean
  public FieldApi fieldApi(EnergyPortal energyPortal) {
    return new FieldApi(energyPortal);
  }

  @Bean
  public UserApi userApi(EnergyPortal energyPortal) {
    return new UserApi(energyPortal);
  }

  public String getUrl() {
    return url;
  }

  public void setUrl(String url) {
    this.url = url;
  }

  public String getPreSharedKey() {
    return preSharedKey;
  }

  public void setPreSharedKey(String preSharedKey) {
    this.preSharedKey = preSharedKey;
  }
}
