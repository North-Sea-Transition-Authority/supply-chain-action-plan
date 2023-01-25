package uk.co.nstauthority.scap.integrationtest;

import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.test.context.DynamicPropertyRegistry;
import org.springframework.test.context.DynamicPropertySource;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.testcontainers.containers.PostgreSQLContainer;

@SuppressWarnings("rawtypes")
@IntegrationTest
@ExtendWith(SpringExtension.class)
public abstract class AbstractIntegrationTest {

  protected static PostgreSQLContainer scapDb;

  @DynamicPropertySource
  private static void addProperties(DynamicPropertyRegistry registry) {
    scapDb = Containers.getOrCreateScapDb();

    registry.add("database.url", scapDb::getJdbcUrl);
    registry.add("schema.password", scapDb::getPassword);
  }
}
