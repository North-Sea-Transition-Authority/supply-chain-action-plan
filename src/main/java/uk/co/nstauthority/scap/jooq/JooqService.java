package uk.co.nstauthority.scap.jooq;

import static org.jooq.impl.DSL.table;

import java.util.Arrays;
import org.jooq.Record;
import org.jooq.Table;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Service;

@Service
public class JooqService {

  /*
  TODO SCAP2022-199: This is a temporary service to make H2 integration tests work, when integration tests are switched
   to postgres this service can be removed
   */

  private final Environment environment;

  @Autowired
  public JooqService(Environment environment) {
    this.environment = environment;
  }

  public Table<Record> scapTable(String tableName) {
    if (Arrays.stream(environment.getActiveProfiles()).anyMatch(
        "integration-test"::equalsIgnoreCase
    )) {
      return table(tableName);
    } else {
      return table("scap.%s".formatted(tableName));
    }
  }
}
