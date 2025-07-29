package uk.co.nstauthority.scap.integrationtest;

import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.test.context.DynamicPropertyRegistry;
import org.springframework.test.context.DynamicPropertySource;
import org.springframework.test.context.bean.override.mockito.MockitoBean;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.testcontainers.containers.PostgreSQLContainer;
import uk.co.fivium.energyportal.accounts.starter.EnergyPortalServiceAccessService;
import uk.co.fivium.energyportalmessagequeue.sns.SnsService;
import uk.co.fivium.energyportalmessagequeue.sqs.SqsService;

@SuppressWarnings("rawtypes")
@IntegrationTest
@ExtendWith(SpringExtension.class)
public abstract class AbstractIntegrationTest {

  @MockitoBean
  private EnergyPortalServiceAccessService energyPortalServiceAccessService;

  @MockitoBean
  private SqsService sqsService;

  @MockitoBean
  private SnsService snsService;

  protected static PostgreSQLContainer scapDb;

  @DynamicPropertySource
  private static void addProperties(DynamicPropertyRegistry registry) {
    scapDb = Containers.getOrCreateScapDb();

    registry.add("database.url", scapDb::getJdbcUrl);
    registry.add("schema.password", scapDb::getPassword);
  }
}
