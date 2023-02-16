package uk.co.nstauthority.scap;

import org.springframework.context.annotation.Import;
import uk.co.nstauthority.scap.authentication.SamlResponseParser;
import uk.co.nstauthority.scap.authentication.ServiceLogoutSuccessHandler;
import uk.co.nstauthority.scap.configuration.SamlProperties;
import uk.co.nstauthority.scap.configuration.WebSecurityConfiguration;

@Import({
    WebSecurityConfiguration.class,
    SamlProperties.class,
    SamlResponseParser.class,
    ServiceLogoutSuccessHandler.class
})
public abstract class AbstractControllerTestWithSecurity extends AbstractControllerTest {
}
