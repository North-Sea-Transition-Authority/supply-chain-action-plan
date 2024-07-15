package uk.co.nstauthority.scap.configuration;

import jakarta.validation.constraints.NotNull;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.validation.annotation.Validated;

@ConfigurationProperties(prefix = "fms")
@Validated
record FeedbackConfigurationProperties(@NotNull String baseUrl,
                                       @NotNull String submitEndpoint,
                                       @NotNull Long timeoutSeconds,
                                       @NotNull String serviceName,
                                       @NotNull String apiKey) {
}
