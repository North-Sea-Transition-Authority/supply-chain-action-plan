package uk.co.nstauthority.scap.technicalsupport;

import javax.validation.constraints.NotNull;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.validation.annotation.Validated;

@ConfigurationProperties(prefix = "technical-support.contact")
@Validated
public record TechnicalSupportConfigurationProperties(
    @NotNull String name,
    @NotNull String phoneNumber,
    @NotNull String emailAddress
) {}
