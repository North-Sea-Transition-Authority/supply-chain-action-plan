package uk.co.nstauthority.scap;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.test.web.servlet.MockMvc;
import uk.co.nstauthority.scap.branding.IncludeServiceBrandingConfigurationProperties;
import uk.co.nstauthority.scap.technicalsupport.IncludeTechnicalSupportConfigurationProperties;

@AutoConfigureMockMvc
@IncludeServiceBrandingConfigurationProperties
@IncludeTechnicalSupportConfigurationProperties
public abstract class AbstractControllerTest {

  @Autowired
  protected MockMvc mockMvc;
}
