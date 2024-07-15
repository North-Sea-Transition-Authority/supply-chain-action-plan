package uk.co.nstauthority.scap;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.boot.context.properties.ConfigurationPropertiesScan;
import org.springframework.scheduling.annotation.EnableScheduling;

@SpringBootApplication
@ConfigurationPropertiesScan
@EntityScan("uk.co.fivium, uk.co.nstauthority.scap")
@EnableScheduling
public class ScapApplication {

  public static void main(String[] args) {
    SpringApplication.run(ScapApplication.class, args);
  }

}
