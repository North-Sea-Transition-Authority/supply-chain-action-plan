package uk.co.nstauthority.scap.configuration;

import org.apache.commons.validator.routines.EmailValidator;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
class EmailValidatorConfiguration {

  @Bean
  EmailValidator emailValidator() {
    return EmailValidator.getInstance();
  }
}
