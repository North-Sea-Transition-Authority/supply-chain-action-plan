package uk.co.nstauthority.scap.permissionmanagement;

import java.util.UUID;
import uk.co.nstauthority.scap.energyportal.EnergyPortalUserDto;
import uk.co.nstauthority.scap.error.exception.IllegalUtilClassInstantiationException;

public class EnergyPortalDtoTestUtil {

  private EnergyPortalDtoTestUtil() {
    throw new IllegalUtilClassInstantiationException(this.getClass());
  }

  public static EnergyPortalDtoBuilder Builder() {
    return new EnergyPortalDtoBuilder();
  }

  public static class EnergyPortalDtoBuilder {

    private Long wuaId = 1000L;
    private TeamType teamType = TeamType.REGULATOR;

    private String teamName = "TestTeam";

    public EnergyPortalDtoBuilder withWuaId(Long wuaId) {
      this.wuaId = wuaId;
      return this;
    }

    public EnergyPortalUserDto build() {
      return new EnergyPortalUserDto(
          wuaId,
          "Dr",
          "Testing",
          "TesterSon",
          "test@test.com",
          "07840885994",
          false,
          true
      );
    }
  }
}