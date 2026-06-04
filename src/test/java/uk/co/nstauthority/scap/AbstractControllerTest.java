package uk.co.nstauthority.scap;

import static org.mockito.Mockito.when;
import static uk.co.nstauthority.scap.authentication.TestUserProvider.getUser;
import static uk.co.nstauthority.scap.authentication.TestUserProvider.user;

import java.time.Clock;
import java.time.Instant;
import java.time.ZoneId;
import java.util.Collections;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.context.MessageSource;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Import;
import org.springframework.context.support.ResourceBundleMessageSource;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.test.context.bean.override.mockito.MockitoBean;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.RequestPostProcessor;
import uk.co.fivium.energyportal.starter.accounts.EnergyPortalAccountsControllerAdvice;
import uk.co.fivium.energyportal.starter.configuration.EnergyPortalAccountsConfigurationProperties;
import uk.co.nstauthority.scap.authentication.SamlResponseParser;
import uk.co.nstauthority.scap.authentication.ServiceLogoutSuccessHandler;
import uk.co.nstauthority.scap.authentication.ServiceUserDetail;
import uk.co.nstauthority.scap.authentication.UserDetailService;
import uk.co.nstauthority.scap.branding.CustomerConfigurationProperties;
import uk.co.nstauthority.scap.branding.IncludeBusinessSupportConfiguration;
import uk.co.nstauthority.scap.branding.IncludeServiceBrandingConfigurationProperties;
import uk.co.nstauthority.scap.configuration.AnalyticsProperties;
import uk.co.nstauthority.scap.configuration.SamlProperties;
import uk.co.nstauthority.scap.configuration.WebMvcConfiguration;
import uk.co.nstauthority.scap.configuration.WebSecurityConfiguration;
import uk.co.nstauthority.scap.controllerhelper.ControllerHelperService;
import uk.co.nstauthority.scap.endpointvalidation.ScapHandlerInterceptor;
import uk.co.nstauthority.scap.endpointvalidation.rules.AnyPermissionForScapRule;
import uk.co.nstauthority.scap.endpointvalidation.rules.AnyPermissionForTeamRule;
import uk.co.nstauthority.scap.endpointvalidation.rules.CanAccessScapRule;
import uk.co.nstauthority.scap.endpointvalidation.rules.MemberOfTeamRule;
import uk.co.nstauthority.scap.endpointvalidation.rules.ScapHasStatusRule;
import uk.co.nstauthority.scap.endpointvalidation.rules.UserHasAnyPermissionRule;
import uk.co.nstauthority.scap.error.FooterService;
import uk.co.nstauthority.scap.fds.navigation.TopNavigationService;
import uk.co.nstauthority.scap.jooq.JooqStatisticsListener;
import uk.co.nstauthority.scap.jpa.HibernateQueryCounterImpl;
import uk.co.nstauthority.scap.mvc.PostAuthenticationRequestMdcFilter;
import uk.co.nstauthority.scap.mvc.RequestLogFilter;
import uk.co.nstauthority.scap.mvc.WithDefaultPageControllerAdvice;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberService;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.scap.ScapService;
import uk.co.nstauthority.scap.technicalsupport.IncludeTechnicalSupportConfigurationProperties;
import uk.co.nstauthority.scap.validation.ValidationErrorOrderingService;

@IncludeServiceBrandingConfigurationProperties
@IncludeTechnicalSupportConfigurationProperties
@IncludeBusinessSupportConfiguration
@EnableConfigurationProperties({
    SamlProperties.class,
    EnergyPortalAccountsConfigurationProperties.class,
})
@Import({
    AbstractControllerTest.TestConfig.class,
    WebSecurityConfiguration.class,
    WebMvcConfiguration.class,
    SamlResponseParser.class,
    ServiceLogoutSuccessHandler.class,
    AnalyticsProperties.class,

    // Interceptor rules
    MemberOfTeamRule.class,
    ScapHasStatusRule.class,
    AnyPermissionForScapRule.class,
    UserHasAnyPermissionRule.class,
    AnyPermissionForTeamRule.class,
    CanAccessScapRule.class,
    ScapHandlerInterceptor.class,
    RequestLogFilter.class,
    PostAuthenticationRequestMdcFilter.class,
    EnergyPortalAccountsControllerAdvice.class,
})
@WithDefaultPageControllerAdvice
@WebMvcTest
public abstract class AbstractControllerTest {

  protected static final ServiceUserDetail testUser = getUser();

  @Autowired
  protected MockMvc mockMvc;

  @MockitoBean
  protected UserDetailService userDetailService;

  @MockitoBean
  protected TeamMemberService teamMemberService;

  @MockitoBean
  protected TopNavigationService topNavigationService;

  @MockitoBean
  protected TeamService teamService;

  @MockitoBean
  protected ScapService scapService;

  @MockitoBean
  protected ScapDetailService scapDetailService;

  @Autowired
  protected FooterService footerService;

  @MockitoBean
  protected JooqStatisticsListener jooqStatisticsListener;

  @BeforeEach
  void setup() {
    when(userDetailService.getUserDetail()).thenReturn(testUser);
    when(teamMemberService.getAllPermissionsForUser(testUser)).thenReturn(List.of(RolePermission.values()));
  }

  public static RequestPostProcessor authenticatedScapUser() {
    return user(testUser, Collections.singleton(new SimpleGrantedAuthority("SCAP_ACCESS_PRIVILEGE")));
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

    @Bean
    public FooterService footerService(CustomerConfigurationProperties customerConfigurationProperties) {
      return new FooterService(customerConfigurationProperties);
    }

    @Bean("messageSource")
    public MessageSource messageSource() {
      ResourceBundleMessageSource messageSource = new ResourceBundleMessageSource();
      messageSource.setBasename("messages");
      messageSource.setDefaultEncoding("UTF-8");
      return messageSource;
    }

    @Bean
    public HibernateQueryCounterImpl hibernateQueryCounter() {
      return new HibernateQueryCounterImpl();
    }

    @Bean
    public Clock clock() {
      return Clock.fixed(Instant.ofEpochSecond(1667576106), ZoneId.systemDefault());
    }
  }
}
