package uk.co.nstauthority.scap.jooq;

import static org.jooq.impl.DSL.table;

import org.jooq.Record;
import org.jooq.Table;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.configuration.JooqConfigurationProperties;

@Service
public class JooqService {

  /*
  TODO SCAP2022-211: Switching from H2 to Postgres simplified this service, when we switch to jOOQ generated types,
  we should be able to remove this service entirely
   */

  private final JooqConfigurationProperties jooqConfigurationProperties;

  @Autowired
  public JooqService(JooqConfigurationProperties jooqConfigurationProperties) {
    this.jooqConfigurationProperties = jooqConfigurationProperties;
  }

  public Table<Record> scapTable(String tableName) {
    return table("%s.%s".formatted(jooqConfigurationProperties.schema(), tableName));
  }
}
