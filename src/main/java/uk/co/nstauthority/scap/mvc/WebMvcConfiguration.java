package uk.co.nstauthority.scap.mvc;

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
import uk.co.nstauthority.scap.permissionmanagement.PermissionManagementHandlerInterceptor;
import uk.co.nstauthority.scap.permissionmanagement.TeamManagementHandlerInterceptor;

@Configuration
public class WebMvcConfiguration implements WebMvcConfigurer {

  private final PermissionManagementHandlerInterceptor permissionManagementHandlerInterceptor;
  private final TeamManagementHandlerInterceptor teamManagementHandlerInterceptor;

  @Autowired
  WebMvcConfiguration(PermissionManagementHandlerInterceptor permissionManagementHandlerInterceptor,
                      TeamManagementHandlerInterceptor teamManagementHandlerInterceptor) {
    this.permissionManagementHandlerInterceptor = permissionManagementHandlerInterceptor;
    this.teamManagementHandlerInterceptor = teamManagementHandlerInterceptor;
  }

  @Override
  public void addResourceHandlers(ResourceHandlerRegistry registry) {
    registry.addResourceHandler("/assets/**")
        .addResourceLocations("classpath:/public/assets/")
        .setCacheControl(CacheControl.maxAge(365, TimeUnit.DAYS))
        .resourceChain(false)
        .addResolver(new VersionResourceResolver().addContentVersionStrategy("/**"));
  }

  @Override
  public void addInterceptors(InterceptorRegistry registry) {
    registry.addInterceptor(new ResponseBufferSizeHandlerInterceptor())
        .excludePathPatterns("/assets/**");
    registry.addInterceptor(permissionManagementHandlerInterceptor)
        .addPathPatterns("/permission-management/**");
    registry.addInterceptor(teamManagementHandlerInterceptor)
        .addPathPatterns("/permission-management/**");
  }

  @Bean
  public ResourceUrlEncodingFilter resourceUrlEncodingFilter() {
    return new ResourceUrlEncodingFilter();
  }
}
