package uk.co.nstauthority.scap.configuration;

import java.time.Clock;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class ClockConfiguration {

  @Bean
  public Clock clock() {
    return Clock.systemDefaultZone();
  }
}
