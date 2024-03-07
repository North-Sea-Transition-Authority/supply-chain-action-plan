package uk.co.nstauthority.scap.branding;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;

@Configuration
public class ServiceBrandingConfigurationProperties {

  private final CustomerConfigurationProperties customerConfigurationProperties;

  private final ServiceConfigurationProperties serviceConfigurationProperties;

  @Autowired
  public ServiceBrandingConfigurationProperties(
      CustomerConfigurationProperties customerConfigurationProperties,
      ServiceConfigurationProperties serviceConfigurationProperties
  ) {
    this.customerConfigurationProperties = customerConfigurationProperties;
    this.serviceConfigurationProperties = serviceConfigurationProperties;
  }

  public CustomerConfigurationProperties getCustomerConfigurationProperties() {
    return customerConfigurationProperties;
  }

  public ServiceConfigurationProperties getServiceConfigurationProperties() {
    return serviceConfigurationProperties;
  }
}
