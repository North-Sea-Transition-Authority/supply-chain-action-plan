package uk.co.nstauthority.scap;

import static org.mockito.Mockito.when;
import static uk.co.nstauthority.scap.authentication.TestUserProvider.getUser;
import static uk.co.nstauthority.scap.authentication.TestUserProvider.user;

import java.time.Clock;
import java.time.Instant;
import java.time.ZoneId;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
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
import org.springframework.test.web.servlet.request.RequestPostProcessor;
import uk.co.nstauthority.scap.authentication.ServiceUserDetail;
import uk.co.nstauthority.scap.authentication.UserDetailService;
import uk.co.nstauthority.scap.branding.IncludeServiceBrandingConfigurationProperties;
import uk.co.nstauthority.scap.controllerhelper.ControllerHelperService;
import uk.co.nstauthority.scap.fds.navigation.TopNavigationService;
import uk.co.nstauthority.scap.mvc.WebMvcConfiguration;
import uk.co.nstauthority.scap.mvc.WithDefaultPageControllerAdvice;
import uk.co.nstauthority.scap.permissionmanagement.PermissionManagementHandlerInterceptor;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.permissionmanagement.TeamPermissionManagementHandlerInterceptor;
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
    TeamPermissionManagementHandlerInterceptor.class,
    TeamManagementHandlerInterceptor.class})
@WithDefaultPageControllerAdvice
@WebMvcTest
public abstract class AbstractControllerTest {

  protected static final ServiceUserDetail testUser = getUser();

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

  @BeforeEach
  void setup() {
    when(userDetailService.getUserDetail()).thenReturn(testUser);
    when(teamMemberService.getAllPermissionsForUser(testUser)).thenReturn(List.of(RolePermission.values()));
  }

  public static RequestPostProcessor testUser() {
    return user(testUser);
  }

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
