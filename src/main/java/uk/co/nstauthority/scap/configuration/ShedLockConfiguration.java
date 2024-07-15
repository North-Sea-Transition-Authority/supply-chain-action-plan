package uk.co.nstauthority.scap.configuration;

import net.javacrumbs.shedlock.core.LockProvider;
import net.javacrumbs.shedlock.provider.jdbctemplate.JdbcTemplateLockProvider;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.jdbc.core.JdbcTemplate;

@Configuration
class ShedLockConfiguration {

  @Bean
  LockProvider lockProvider(JdbcTemplate jdbcTemplate) {
    return new JdbcTemplateLockProvider(
        JdbcTemplateLockProvider.Configuration.builder()
            .withJdbcTemplate(jdbcTemplate)
            .withTableName("scap.shedlock")
            .usingDbTime()
            .build()
    );
  }
}
