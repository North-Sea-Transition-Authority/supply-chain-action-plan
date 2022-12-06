package uk.co.nstauthority.scap.scap.scap;

import java.time.Instant;
import org.springframework.data.repository.CrudRepository;

public interface ScapRepository extends CrudRepository<Scap, Integer> {

  int countByCreatedTimestampBetween(Instant startInstant, Instant endInstant);

}
