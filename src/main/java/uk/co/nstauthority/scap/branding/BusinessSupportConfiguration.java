package uk.co.nstauthority.scap.branding;

import javax.validation.constraints.NotNull;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.validation.annotation.Validated;

@ConfigurationProperties(prefix = "business-support.contact")
@Validated
public record BusinessSupportConfiguration(

    @NotNull String name,
    @NotNull String phoneNumber,
    @NotNull String emailAddress
) {
}
