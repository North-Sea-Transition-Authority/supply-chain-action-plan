package uk.co.nstauthority.scap.permissionmanagement;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import java.util.UUID;
import org.junit.jupiter.api.Test;
import org.springframework.web.server.ResponseStatusException;

class TeamIdTest {

  @Test
  void valueOf_String() {
    var uuid = UUID.randomUUID();
    var teamId = TeamId.valueOf(uuid.toString());
    assertThat(teamId.uuid()).isEqualTo(uuid);
  }

  @Test
  void valueOf_Uuid() {
    var uuid = UUID.randomUUID();
    var teamId = TeamId.valueOf(uuid);
    assertThat(teamId.uuid()).isEqualTo(uuid);
  }

  @Test
  void valueOf_InvalidUUID() {
    var nonUUIDValue = "non numeric string";
    assertThatThrownBy(() -> TeamId.valueOf(nonUUIDValue))
        .isInstanceOf(ResponseStatusException.class)
        .hasMessageContaining("Cannot find Team with ID: %s".formatted(nonUUIDValue));
  }
}