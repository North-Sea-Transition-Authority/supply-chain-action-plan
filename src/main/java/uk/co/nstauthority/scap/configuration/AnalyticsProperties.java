package uk.co.nstauthority.scap.configuration;

import javax.validation.constraints.NotNull;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.client.ClientHttpRequestFactory;
import org.springframework.http.client.SimpleClientHttpRequestFactory;
import org.springframework.validation.annotation.Validated;
import uk.co.nstauthority.scap.util.ProxyUtil;

@Configuration
@Validated
@ConfigurationProperties(prefix = "analytics")
public class AnalyticsProperties {

  @NotNull
  private String appTag;

  @NotNull
  private String globalTag;

  @NotNull
  private boolean enabled;

  private String apiSecret;

  private String endpointUrl;

  private String userAgent;

  private Integer connectionTimeoutSeconds;

  @Bean
  public ClientHttpRequestFactory requestFactory(@Value("${proxy.host:#{null}}") String proxyHost,
                                                 @Value("${proxy.port:#{null}}") String proxyPort) {
    var httpRequestFactory = new SimpleClientHttpRequestFactory();
    var proxy = ProxyUtil.createProxy(proxyHost, proxyPort);
    httpRequestFactory.setProxy(proxy);
    return httpRequestFactory;
  }


  public String getAppTag() {
    return appTag;
  }

  public void setAppTag(String appTag) {
    this.appTag = appTag;
  }

  public String getGlobalTag() {
    return globalTag;
  }

  public void setGlobalTag(String globalTag) {
    this.globalTag = globalTag;
  }

  public boolean isEnabled() {
    return enabled;
  }

  public void setEnabled(boolean enabled) {
    this.enabled = enabled;
  }

  public String getApiSecret() {
    return apiSecret;
  }

  public void setApiSecret(String apiSecret) {
    this.apiSecret = apiSecret;
  }

  public String getEndpointUrl() {
    return endpointUrl;
  }

  public void setEndpointUrl(String endpointUrl) {
    this.endpointUrl = endpointUrl;
  }

  public String getUserAgent() {
    return userAgent;
  }

  public void setUserAgent(String userAgent) {
    this.userAgent = userAgent;
  }

  public Integer getConnectionTimeoutSeconds() {
    return connectionTimeoutSeconds;
  }

  public void setConnectionTimeoutSeconds(Integer connectionTimeoutSeconds) {
    this.connectionTimeoutSeconds = connectionTimeoutSeconds;
  }
}
