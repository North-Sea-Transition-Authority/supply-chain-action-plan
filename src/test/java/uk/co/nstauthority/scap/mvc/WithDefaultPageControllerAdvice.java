package uk.co.nstauthority.scap.mvc;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import org.springframework.test.context.ContextConfiguration;
import uk.co.nstauthority.scap.branding.IncludeServiceBrandingConfigurationProperties;

@ContextConfiguration(classes = {
    DefaultPageControllerAdvice.class,
})
@IncludeServiceBrandingConfigurationProperties
@Retention(RetentionPolicy.RUNTIME)
public @interface WithDefaultPageControllerAdvice {
}