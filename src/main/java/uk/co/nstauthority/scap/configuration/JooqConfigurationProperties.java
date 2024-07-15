package uk.co.nstauthority.scap.configuration;

import jakarta.validation.constraints.NotNull;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.validation.annotation.Validated;

@ConfigurationProperties(prefix = "jooq")
@Validated
public record JooqConfigurationProperties(
    @NotNull String schema
) {
}
