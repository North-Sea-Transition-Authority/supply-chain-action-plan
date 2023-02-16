package uk.co.nstauthority.scap.configuration;

import org.springframework.boot.context.properties.ConfigurationProperties;

@ConfigurationProperties(prefix = "email")
public record EmailConfiguration(String mode,
                                 String testRecipientList,
                                 String callbackEmail) {}
