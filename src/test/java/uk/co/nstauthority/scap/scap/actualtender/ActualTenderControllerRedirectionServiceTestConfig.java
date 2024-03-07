package uk.co.nstauthority.scap.scap.actualtender;

import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.context.annotation.Bean;

@TestConfiguration
public class ActualTenderControllerRedirectionServiceTestConfig {

  @Bean
  public ActualTenderControllerRedirectionService actualTenderControllerRedirectionService() {
    return new ActualTenderControllerRedirectionService();
  }
}
