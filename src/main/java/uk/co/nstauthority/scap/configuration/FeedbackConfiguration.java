package uk.co.nstauthority.scap.configuration;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import uk.co.fivium.feedbackmanagementservice.client.FeedbackClientService;

@Configuration
class FeedbackConfiguration {

  @Bean
  FeedbackClientService feedbackClientService(FeedbackConfigurationProperties feedbackConfigurationProperties,
                                              ObjectMapper objectMapper) {
    return new FeedbackClientService(
        objectMapper,
        feedbackConfigurationProperties.baseUrl(),
        feedbackConfigurationProperties.timeoutSeconds(),
        feedbackConfigurationProperties.submitEndpoint(),
        feedbackConfigurationProperties.serviceName(),
        feedbackConfigurationProperties.apiKey()
    );
  }
}
