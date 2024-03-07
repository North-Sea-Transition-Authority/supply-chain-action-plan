package uk.co.nstauthority.scap.error;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;
import uk.co.nstauthority.scap.branding.ServiceBrandingConfigurationProperties;
import uk.co.nstauthority.scap.technicalsupport.TechnicalSupportConfigurationProperties;

@Configuration
public class ErrorConfiguration {

  private final ErrorConfigurationProperties errorConfigurationProperties;

  private final ServiceBrandingConfigurationProperties serviceBrandingConfigurationProperties;
  private final TechnicalSupportConfigurationProperties technicalSupportConfigurationProperties;


  @Autowired
  public ErrorConfiguration(ErrorConfigurationProperties errorConfigurationProperties,
                            ServiceBrandingConfigurationProperties serviceBrandingConfigurationProperties,
                            TechnicalSupportConfigurationProperties technicalSupportConfigurationProperties) {
    this.errorConfigurationProperties = errorConfigurationProperties;
    this.serviceBrandingConfigurationProperties = serviceBrandingConfigurationProperties;
    this.technicalSupportConfigurationProperties = technicalSupportConfigurationProperties;
  }

  public ErrorConfigurationProperties getErrorConfigurationProperties() {
    return errorConfigurationProperties;
  }

  public ServiceBrandingConfigurationProperties getServiceBrandingConfigurationProperties() {
    return serviceBrandingConfigurationProperties;
  }

  public TechnicalSupportConfigurationProperties getTechnicalSupportConfigurationProperties() {
    return technicalSupportConfigurationProperties;
  }
}
