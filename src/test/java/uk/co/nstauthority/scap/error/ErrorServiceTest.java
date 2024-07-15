package uk.co.nstauthority.scap.error;

import static org.assertj.core.api.Assertions.assertThat;

import jakarta.servlet.http.HttpServletRequest;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.web.servlet.ModelAndView;
import uk.co.nstauthority.scap.branding.CustomerConfigurationProperties;
import uk.co.nstauthority.scap.branding.ServiceBrandingConfigurationProperties;
import uk.co.nstauthority.scap.branding.ServiceConfigurationProperties;
import uk.co.nstauthority.scap.configuration.AnalyticsProperties;
import uk.co.nstauthority.scap.fds.navigation.TopNavigationService;
import uk.co.nstauthority.scap.technicalsupport.TechnicalSupportConfigurationProperties;

@ExtendWith(SpringExtension.class)
@EnableConfigurationProperties(value = {
    ErrorConfigurationProperties.class,
    ServiceConfigurationProperties.class,
    CustomerConfigurationProperties.class,
    TechnicalSupportConfigurationProperties.class
})
@ContextConfiguration(classes = {ErrorConfiguration.class, ServiceBrandingConfigurationProperties.class})
@TestPropertySource("classpath:application-test.properties")
class ErrorServiceTest {

  private static final String ERROR_REF_ATTRIBUTE_NAME = "errorRef";
  private static final String SERVICE_HOME_URLS_ATTRIBUTE_NAME = "serviceHomeUrl";
  private static final String STACK_TRACE_ATTRIBUTE = "stackTrace";
  private static final String UNSAFE_CHARACTERS = "0125AEIOULNSZ";

  private static final String SERVICE_BRANDING_ATTRIBUTE = "serviceBranding";

  private static final String CUSTOMER_BRANDING_ATTRIBUTE = "customerBranding";

  private static final String ANALYTICS_ATTRIBUTE = "analytics";

  private static final String TECHNICAL_SUPPORT_ATTRIBUTE = "technicalSupport";

  @Autowired
  private ErrorConfiguration errorConfiguration;

  @Autowired
  private ServiceBrandingConfigurationProperties serviceBrandingConfigurationProperties;

  @MockBean
  private TopNavigationService topNavigationService;

  @MockBean
  private AnalyticsProperties analyticsProperties;

  @MockBean
  private FooterService footerService;

  @MockBean
  private HttpServletRequest request;

  private ErrorService errorService;

  @BeforeEach
  public void setup() {
    errorService = new ErrorService(errorConfiguration, footerService, analyticsProperties, topNavigationService);
  }

  @Test
  void addErrorAttributesToModel_whenThrowableError_assertExpectedModelAttributes() {
    final var resultingModelMap =  errorService.addErrorAttributesToModel(
        new ModelAndView(),
        new NullPointerException(),
        request
    ).getModelMap();

    assertThat(resultingModelMap).containsKeys(
        ERROR_REF_ATTRIBUTE_NAME,
        SERVICE_HOME_URLS_ATTRIBUTE_NAME,
        STACK_TRACE_ATTRIBUTE,
        SERVICE_BRANDING_ATTRIBUTE,
        CUSTOMER_BRANDING_ATTRIBUTE,
        TECHNICAL_SUPPORT_ATTRIBUTE,
        ANALYTICS_ATTRIBUTE
    );
    Object errorRef = resultingModelMap.get(ERROR_REF_ATTRIBUTE_NAME);
    assertThat(errorRef).isNotNull();
    assertThat((String) errorRef).doesNotContainPattern(UNSAFE_CHARACTERS);
    assertThat(resultingModelMap.get(STACK_TRACE_ATTRIBUTE)).isNotNull();
  }

  @Test
  void addErrorAttributesToModel_whenNoThrowableError_assertExpectedModelAttributes() {
    final var resultingModelMap =  errorService.addErrorAttributesToModel(
        new ModelAndView(),
        null,
        request).getModelMap();

    assertThat(resultingModelMap).containsKeys(
        SERVICE_HOME_URLS_ATTRIBUTE_NAME,
        TECHNICAL_SUPPORT_ATTRIBUTE,
        SERVICE_BRANDING_ATTRIBUTE,
        CUSTOMER_BRANDING_ATTRIBUTE,
        ANALYTICS_ATTRIBUTE
    );
  }
}
