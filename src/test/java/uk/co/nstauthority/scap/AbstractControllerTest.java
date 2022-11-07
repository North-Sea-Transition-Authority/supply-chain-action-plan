package uk.co.nstauthority.scap;

import java.time.Clock;
import java.time.Instant;
import java.time.ZoneId;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.context.MessageSource;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Import;
import org.springframework.context.support.ResourceBundleMessageSource;
import org.springframework.test.web.servlet.MockMvc;
import uk.co.nstauthority.scap.branding.IncludeServiceBrandingConfigurationProperties;
import uk.co.nstauthority.scap.controllerhelper.ControllerHelperService;
import uk.co.nstauthority.scap.technicalsupport.IncludeTechnicalSupportConfigurationProperties;
import uk.co.nstauthority.scap.validation.ValidationErrorOrderingService;

@AutoConfigureMockMvc
@IncludeServiceBrandingConfigurationProperties
@IncludeTechnicalSupportConfigurationProperties
@Import(AbstractControllerTest.TestConfig.class)
public abstract class AbstractControllerTest {

  @Autowired
  protected MockMvc mockMvc;

  @TestConfiguration
  public static class TestConfig {
    @Bean
    public ControllerHelperService controllerHelperService() {
      return new ControllerHelperService(validationErrorOrderingService());
    }

    @Bean
    public ValidationErrorOrderingService validationErrorOrderingService() {
      return new ValidationErrorOrderingService(messageSource());
    }

    @Bean("messageSource")
    public MessageSource messageSource() {
      ResourceBundleMessageSource messageSource = new ResourceBundleMessageSource();
      messageSource.setBasename("messages");
      messageSource.setDefaultEncoding("UTF-8");
      return messageSource;
    }

    @Bean
    public Clock clock() {
      return Clock.fixed(Instant.ofEpochSecond(1667576106), ZoneId.systemDefault());
    }
  }
}
