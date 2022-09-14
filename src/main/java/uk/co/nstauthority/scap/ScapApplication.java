package uk.co.nstauthority.scap;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.context.properties.ConfigurationPropertiesScan;

@SpringBootApplication
@ConfigurationPropertiesScan
public class ScapApplication {

  public static void main(String[] args) {
    SpringApplication.run(ScapApplication.class, args);
  }

}