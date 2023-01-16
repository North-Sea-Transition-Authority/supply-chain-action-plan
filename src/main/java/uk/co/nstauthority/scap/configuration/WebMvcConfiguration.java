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
import uk.co.nstauthority.scap.mvc.ResponseBufferSizeHandlerInterceptor;
import uk.co.nstauthority.scap.permissionmanagement.endpointsecurity.PermissionManagementHandlerInterceptor;
import uk.co.nstauthority.scap.permissionmanagement.endpointsecurity.ScapPermissionManagementHandlerInterceptor;
import uk.co.nstauthority.scap.permissionmanagement.endpointsecurity.TeamManagementHandlerInterceptor;
import uk.co.nstauthority.scap.permissionmanagement.endpointsecurity.TeamPermissionManagementHandlerInterceptor;

@Configuration
public class WebMvcConfiguration implements WebMvcConfigurer {

  private static final String ASSET_EXCLUSION_PATH = "/assets/**";

  private final TeamPermissionManagementHandlerInterceptor teamPermissionManagementHandlerInterceptor;
  private final TeamManagementHandlerInterceptor teamManagementHandlerInterceptor;

  private final PermissionManagementHandlerInterceptor permissionManagementHandlerInterceptor;

  private final ScapPermissionManagementHandlerInterceptor scapPermissionManagementHandlerInterceptor;
  private final ScapHandlerInterceptor scapHandlerInterceptor;

  @Autowired
  WebMvcConfiguration(TeamPermissionManagementHandlerInterceptor teamPermissionManagementHandlerInterceptor,
                      TeamManagementHandlerInterceptor teamManagementHandlerInterceptor,
                      PermissionManagementHandlerInterceptor permissionManagementHandlerInterceptor,
                      ScapPermissionManagementHandlerInterceptor scapPermissionManagementHandlerInterceptor,
                      ScapHandlerInterceptor scapHandlerInterceptor) {
    this.teamPermissionManagementHandlerInterceptor = teamPermissionManagementHandlerInterceptor;
    this.teamManagementHandlerInterceptor = teamManagementHandlerInterceptor;
    this.permissionManagementHandlerInterceptor = permissionManagementHandlerInterceptor;
    this.scapPermissionManagementHandlerInterceptor = scapPermissionManagementHandlerInterceptor;
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
        .excludePathPatterns(ASSET_EXCLUSION_PATH);
    registry.addInterceptor(permissionManagementHandlerInterceptor)
        .excludePathPatterns(ASSET_EXCLUSION_PATH);
    registry.addInterceptor(scapPermissionManagementHandlerInterceptor)
        .excludePathPatterns(ASSET_EXCLUSION_PATH);
    registry.addInterceptor(teamPermissionManagementHandlerInterceptor)
        .addPathPatterns("/permission-management/**");
    registry.addInterceptor(scapHandlerInterceptor)
        .excludePathPatterns(ASSET_EXCLUSION_PATH);
    registry.addInterceptor(teamManagementHandlerInterceptor)
        .addPathPatterns("/permission-management/**");
    // TODO SCAP2022-203: Condense interceptors to just teamHandlerInterceptor and scapHandlerInterceptor

  }

  @Bean
  public ResourceUrlEncodingFilter resourceUrlEncodingFilter() {
    return new ResourceUrlEncodingFilter();
  }
}
