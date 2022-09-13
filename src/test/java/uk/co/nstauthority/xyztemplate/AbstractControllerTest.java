package uk.co.nstauthority.xyztemplate;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.test.web.servlet.MockMvc;
import uk.co.nstauthority.xyztemplate.branding.IncludeServiceBrandingConfigurationProperties;

@AutoConfigureMockMvc
@IncludeServiceBrandingConfigurationProperties
public abstract class AbstractControllerTest {

  @Autowired
  protected MockMvc mockMvc;
}
