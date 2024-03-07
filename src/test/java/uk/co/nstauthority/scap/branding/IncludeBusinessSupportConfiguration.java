package uk.co.nstauthority.scap.branding;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import org.springframework.boot.context.properties.EnableConfigurationProperties;

@EnableConfigurationProperties(BusinessSupportConfiguration.class)
@Retention(RetentionPolicy.RUNTIME)
public @interface IncludeBusinessSupportConfiguration {
}
