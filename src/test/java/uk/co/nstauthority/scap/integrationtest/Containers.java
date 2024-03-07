package uk.co.nstauthority.scap.integrationtest;

import org.slf4j.LoggerFactory;
import org.testcontainers.containers.Network;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.containers.output.Slf4jLogConsumer;
import org.testcontainers.utility.DockerImageName;

@SuppressWarnings({"rawtypes", "unchecked"})
public class Containers {
  private static final Network NETWORK = Network.newNetwork();
  private static PostgreSQLContainer SCAP_DB;

  public static PostgreSQLContainer getOrCreateScapDb() {
    if (SCAP_DB == null) {
      try {
        startUp();
      } catch (Throwable t) {
        throw new Error(t);
      }
    }
    return SCAP_DB;
  }

  private static PostgreSQLContainer startUpDbContainer(Slf4jLogConsumer logConsumer) {
    PostgreSQLContainer dbContainer = new PostgreSQLContainer(DockerImageName.parse("postgres:14.2-alpine"))
        .withDatabaseName("scap")
        .withUsername("scap_app");
    dbContainer.withNetwork(Containers.NETWORK).withNetworkAliases("database");
    dbContainer.start();
    dbContainer.followOutput(logConsumer);
    return dbContainer;
  }

  private static void startUp() {

    Slf4jLogConsumer dbLogConsumer = new Slf4jLogConsumer(LoggerFactory.getLogger("Postgres"));
    SCAP_DB = startUpDbContainer(dbLogConsumer);
  }
}

