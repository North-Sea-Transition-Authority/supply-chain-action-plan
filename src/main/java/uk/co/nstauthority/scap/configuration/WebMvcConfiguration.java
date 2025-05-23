package uk.co.nstauthority.scap.configuration;

import java.util.concurrent.TimeUnit;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.CacheControl;
import org.springframework.web.servlet.config.annotation.InterceptorRegistry;
import org.springframework.web.servlet.config.annotation.ResourceHandlerRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;
import org.springframework.web.servlet.resource.ResourceUrlEncodingFilter;
import org.springframework.web.servlet.resource.VersionResourceResolver;
import uk.co.nstauthority.scap.endpointvalidation.ScapHandlerInterceptor;
import uk.co.nstauthority.scap.endpointvalidation.rules.AnyPermissionForTeamRule;
import uk.co.nstauthority.scap.mvc.ResponseBufferSizeHandlerInterceptor;

@Configuration
public class WebMvcConfiguration implements WebMvcConfigurer {

  private static final String ASSET_EXCLUSION_PATH = "/assets/**";

  private static final String ERROR_EXCLUSION_PATH = "/error/**";

  private final ScapHandlerInterceptor scapHandlerInterceptor;

  @Autowired
  WebMvcConfiguration(AnyPermissionForTeamRule permissionForTeamRule,
                      ScapHandlerInterceptor scapHandlerInterceptor) {
    this.scapHandlerInterceptor = scapHandlerInterceptor;
  }

  @Override
  public void addResourceHandlers(ResourceHandlerRegistry registry) {
    registry.addResourceHandler(ASSET_EXCLUSION_PATH)
        .addResourceLocations("classpath:/public/assets/")
        .setCacheControl(CacheControl.maxAge(365, TimeUnit.DAYS))
        .resourceChain(false)
        .addResolver(new VersionResourceResolver().addContentVersionStrategy("/**"));
  }

  @Override
  public void addInterceptors(InterceptorRegistry registry) {
    registry.addInterceptor(new ResponseBufferSizeHandlerInterceptor())
        .excludePathPatterns(ASSET_EXCLUSION_PATH, ERROR_EXCLUSION_PATH);
    registry.addInterceptor(scapHandlerInterceptor)
        .excludePathPatterns(ASSET_EXCLUSION_PATH, ERROR_EXCLUSION_PATH);
  }

  @Bean
  public ResourceUrlEncodingFilter resourceUrlEncodingFilter() {
    return new ResourceUrlEncodingFilter();
  }
}
