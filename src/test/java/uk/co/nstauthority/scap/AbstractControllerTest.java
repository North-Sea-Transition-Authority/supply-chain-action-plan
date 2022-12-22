package uk.co.nstauthority.scap;

import java.time.Clock;
import java.time.Instant;
import java.time.ZoneId;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.MessageSource;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Import;
import org.springframework.context.support.ResourceBundleMessageSource;
import org.springframework.test.web.servlet.MockMvc;
import uk.co.nstauthority.scap.authentication.UserDetailService;
import uk.co.nstauthority.scap.branding.IncludeServiceBrandingConfigurationProperties;
import uk.co.nstauthority.scap.configuration.SamlProperties;
import uk.co.nstauthority.scap.controllerhelper.ControllerHelperService;
import uk.co.nstauthority.scap.fds.navigation.TopNavigationService;
import uk.co.nstauthority.scap.mvc.WebMvcConfiguration;
import uk.co.nstauthority.scap.mvc.WithDefaultPageControllerAdvice;
import uk.co.nstauthority.scap.permissionmanagement.PermissionManagementHandlerInterceptor;
import uk.co.nstauthority.scap.permissionmanagement.TeamManagementHandlerInterceptor;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberService;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;
import uk.co.nstauthority.scap.technicalsupport.IncludeTechnicalSupportConfigurationProperties;
import uk.co.nstauthority.scap.validation.ValidationErrorOrderingService;

@AutoConfigureMockMvc
@IncludeServiceBrandingConfigurationProperties
@IncludeTechnicalSupportConfigurationProperties
@Import({
    AbstractControllerTest.TestConfig.class,
    WebMvcConfiguration.class,
    PermissionManagementHandlerInterceptor.class,
    TeamManagementHandlerInterceptor.class})
@WithDefaultPageControllerAdvice
@WebMvcTest
public abstract class AbstractControllerTest {

  @Autowired
  protected MockMvc mockMvc;

  @MockBean
  protected UserDetailService userDetailService;

  @MockBean
  protected TeamMemberService teamMemberService;

  @MockBean
  TopNavigationService topNavigationService;

  @MockBean
  protected TeamService teamService;

  @TestConfiguration
  public static class TestConfig {
    @Bean
    public ControllerHelperService controllerHelperService() {
      return new ControllerHelperService(validationErrorOrderingService());
    }

    @Bean
    public ValidationErrorOrderingService validationErrorOrderingService() {
      return new ValidationErrorOrderingService(messageSource());
    }

    @Bean("messageSource")
    public MessageSource messageSource() {
      ResourceBundleMessageSource messageSource = new ResourceBundleMessageSource();
      messageSource.setBasename("messages");
      messageSource.setDefaultEncoding("UTF-8");
      return messageSource;
    }

    @Bean
    public Clock clock() {
      return Clock.fixed(Instant.ofEpochSecond(1667576106), ZoneId.systemDefault());
    }
  }
}
