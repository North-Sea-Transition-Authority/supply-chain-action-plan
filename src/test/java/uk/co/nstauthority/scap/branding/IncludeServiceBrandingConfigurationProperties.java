package uk.co.nstauthority.scap.branding;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Import;
import org.springframework.test.context.ActiveProfiles;

@ActiveProfiles("test")
@EnableConfigurationProperties(value = {
    CustomerConfigurationProperties.class,
    ServiceConfigurationProperties.class
})
@Import(ServiceBrandingConfigurationProperties.class)
@Retention(RetentionPolicy.RUNTIME)
public @interface IncludeServiceBrandingConfigurationProperties {
}
