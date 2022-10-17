package uk.co.nstauthority.scap.technicalsupport;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;

@Configuration
public class TechnicalSupportConfiguration {

  private final TechnicalSupportConfigurationProperties technicalSupportConfigurationProperties;

  @Autowired
  public TechnicalSupportConfiguration(
      TechnicalSupportConfigurationProperties technicalSupportConfigurationProperties) {
    this.technicalSupportConfigurationProperties = technicalSupportConfigurationProperties;
  }

  public TechnicalSupportConfigurationProperties getTechnicalSupportConfigurationProperties() {
    return technicalSupportConfigurationProperties;
  }
}
