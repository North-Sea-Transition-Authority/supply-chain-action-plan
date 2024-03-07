package uk.co.nstauthority.scap.configuration;

import javax.validation.constraints.NotNull;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.validation.annotation.Validated;
import uk.co.fivium.digital.energyportalteamaccesslibrary.team.EnergyPortalAccessService;
import uk.co.fivium.energyportalapi.client.EnergyPortal;
import uk.co.fivium.energyportalapi.client.countries.CountryApi;
import uk.co.fivium.energyportalapi.client.facility.FacilityApi;
import uk.co.fivium.energyportalapi.client.field.FieldApi;
import uk.co.fivium.energyportalapi.client.organisation.OrganisationApi;
import uk.co.fivium.energyportalapi.client.pathfinder.PathfinderApi;
import uk.co.fivium.energyportalapi.client.user.UserApi;

@Configuration
@ConfigurationProperties(prefix = "energy-portal-api")
@Validated
public class EnergyPortalApiConfiguration {

  @NotNull String url;
  @NotNull String preSharedKey;

  @NotNull String accessUrl;

  @NotNull String accessPreSharedKey;

  @Bean
  public EnergyPortalAccessService energyPortalAccessService() {
    return new EnergyPortalAccessService(accessUrl, accessPreSharedKey);
  }

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

  @Bean
  public FacilityApi facilityApi(EnergyPortal energyPortal) {
    return new FacilityApi(energyPortal);
  }

  @Bean
  public PathfinderApi pathfinderApi(EnergyPortal energyPortal) {
    return new PathfinderApi(energyPortal);
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

  public String getAccessUrl() {
    return accessUrl;
  }

  public void setAccessUrl(String accessUrl) {
    this.accessUrl = accessUrl;
  }

  public String getAccessPreSharedKey() {
    return accessPreSharedKey;
  }

  public void setAccessPreSharedKey(String accessPreSharedKey) {
    this.accessPreSharedKey = accessPreSharedKey;
  }
}
