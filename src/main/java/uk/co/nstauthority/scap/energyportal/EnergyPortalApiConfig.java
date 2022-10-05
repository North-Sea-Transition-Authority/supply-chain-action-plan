package uk.co.nstauthority.scap.energyportal;

import javax.validation.constraints.NotNull;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;
import org.springframework.validation.annotation.Validated;
@ConfigurationProperties(prefix = "energy-portal-api")
@Validated
public record EnergyPortalApiConfig(
    @NotNull String url,
    @NotNull String preSharedKey,

    boolean sslVerification
) {}
