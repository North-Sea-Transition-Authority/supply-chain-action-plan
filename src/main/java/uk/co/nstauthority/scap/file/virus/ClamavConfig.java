package uk.co.nstauthority.scap.file.virus;

import org.springframework.boot.context.properties.ConfigurationProperties;

@ConfigurationProperties(prefix = "clamav")
public record ClamavConfig(String host, int port, int timeout) { }