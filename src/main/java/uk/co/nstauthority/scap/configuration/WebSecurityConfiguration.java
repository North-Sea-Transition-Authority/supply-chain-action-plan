package uk.co.nstauthority.scap.configuration;

import java.io.ByteArrayInputStream;
import java.nio.charset.StandardCharsets;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.util.Objects;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.authentication.ProviderManager;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.saml2.core.Saml2X509Credential;
import org.springframework.security.saml2.provider.service.authentication.OpenSaml4AuthenticationProvider;
import org.springframework.security.saml2.provider.service.registration.InMemoryRelyingPartyRegistrationRepository;
import org.springframework.security.saml2.provider.service.registration.RelyingPartyRegistration;
import org.springframework.security.saml2.provider.service.registration.RelyingPartyRegistrationRepository;
import org.springframework.security.saml2.provider.service.registration.Saml2MessageBinding;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.access.AccessDeniedHandler;
import org.springframework.security.web.context.SecurityContextHolderFilter;
import uk.co.nstauthority.scap.authentication.SamlResponseParser;
import uk.co.nstauthority.scap.authentication.ServiceLogoutSuccessHandler;
import uk.co.nstauthority.scap.mvc.PostAuthenticationRequestMdcFilter;
import uk.co.nstauthority.scap.mvc.RequestLogFilter;

@Configuration
public class WebSecurityConfiguration {

  private final SamlProperties samlProperties;

  private final SamlResponseParser samlResponseParser;

  private final ServiceLogoutSuccessHandler serviceLogoutSuccessHandler;
  private final RequestLogFilter requestLogFilter;
  private final PostAuthenticationRequestMdcFilter postAuthenticationRequestMdcFilter;

  private static final String SCAP_ACCESS_PERMISSION = "SCAP_ACCESS_PRIVILEGE";

  @Autowired
  public WebSecurityConfiguration(SamlProperties samlProperties,
                                  SamlResponseParser samlResponseParser,
                                  ServiceLogoutSuccessHandler serviceLogoutSuccessHandler, RequestLogFilter requestLogFilter,
                                  PostAuthenticationRequestMdcFilter postAuthenticationRequestMdcFilter) {
    this.samlProperties = samlProperties;
    this.samlResponseParser = samlResponseParser;
    this.serviceLogoutSuccessHandler = serviceLogoutSuccessHandler;
    this.requestLogFilter = requestLogFilter;
    this.postAuthenticationRequestMdcFilter = postAuthenticationRequestMdcFilter;
  }

  @Bean
  protected SecurityFilterChain filterChain(HttpSecurity httpSecurity) throws Exception {
    var authenticationProvider = new OpenSaml4AuthenticationProvider();
    authenticationProvider.setResponseAuthenticationConverter(r -> samlResponseParser.parseSamlResponse(r.getResponse()));

    httpSecurity
        .authorizeHttpRequests(http -> http
            .requestMatchers(
                "/assets/**",
                "/notify/callback",
                "/api/v1/logout/*",
                "/actuator/health"
            )
            .permitAll()
            .requestMatchers("/*").hasAuthority(SCAP_ACCESS_PERMISSION)
            .anyRequest().authenticated()
        )
        .saml2Login(saml2 -> saml2.authenticationManager(new ProviderManager(authenticationProvider)))
        .logout(logout -> logout.logoutSuccessHandler(serviceLogoutSuccessHandler))
        .csrf(csrf -> csrf.ignoringRequestMatchers("/notify/callback", "/api/v1/logout/*"))
        .securityContext(securityContext -> securityContext.requireExplicitSave(true))
        .exceptionHandling(exceptionHandling -> exceptionHandling.accessDeniedHandler(serviceDeniedHandler()))
        .addFilterBefore(requestLogFilter, SecurityContextHolderFilter.class)
        .addFilterAfter(postAuthenticationRequestMdcFilter, SecurityContextHolderFilter.class);

    return httpSecurity.build();
  }

  @Bean
  protected RelyingPartyRegistrationRepository relyingPartyRegistrations() throws CertificateException {
    var registration = getRelyingPartyRegistration();
    return new InMemoryRelyingPartyRegistrationRepository(registration);
  }

  @Bean
  protected AccessDeniedHandler serviceDeniedHandler() {
    return new ServiceDeniedHandler();
  }

  @Bean
  public RelyingPartyRegistration getRelyingPartyRegistration() throws CertificateException {

    var certificateStream = new ByteArrayInputStream(samlProperties.getCertificate().getBytes(StandardCharsets.UTF_8));

    var certificate = (X509Certificate) CertificateFactory.getInstance("X.509")
        .generateCertificate(certificateStream);

    var credential = Saml2X509Credential.verification(Objects.requireNonNull(certificate));

    return RelyingPartyRegistration
        .withRegistrationId(samlProperties.getRegistrationId())
        .assertingPartyDetails(party -> party
            .entityId(samlProperties.getEntityId())
            .singleSignOnServiceLocation(samlProperties.getLoginUrl())
            .singleSignOnServiceBinding(Saml2MessageBinding.POST)
            .wantAuthnRequestsSigned(false)
            .verificationX509Credentials(c -> c.add(credential))
        )
        .assertionConsumerServiceLocation(samlProperties.getConsumerServiceLocation())
        .build();
  }

}
