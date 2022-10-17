package uk.co.nstauthority.scap.error;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;

@Configuration
public class ErrorConfiguration {

  private final ErrorConfigurationProperties errorConfigurationProperties;

  @Autowired
  public ErrorConfiguration(ErrorConfigurationProperties errorConfigurationProperties) {
    this.errorConfigurationProperties = errorConfigurationProperties;
  }

  public ErrorConfigurationProperties getErrorConfigurationProperties() {
    return errorConfigurationProperties;
  }
}
