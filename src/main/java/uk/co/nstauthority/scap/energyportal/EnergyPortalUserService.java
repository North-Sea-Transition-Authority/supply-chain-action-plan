package uk.co.nstauthority.scap.energyportal;

import java.util.List;
import java.util.Optional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.fivium.energyportalapi.client.user.UserApi;
import uk.co.fivium.energyportalapi.generated.client.UserProjectionRoot;
import uk.co.fivium.energyportalapi.generated.client.UsersProjectionRoot;
import uk.co.fivium.energyportalapi.generated.types.User;

@Service
public class EnergyPortalUserService {

  static final UsersProjectionRoot USERS_PROJECTION_ROOT = new UsersProjectionRoot()
      .webUserAccountId()
      .title()
      .forename()
      .surname()
      .primaryEmailAddress()
      .telephoneNumber()
      .canLogin()
      .isAccountShared();

  static final UserProjectionRoot USER_PROJECTION_ROOT = new UserProjectionRoot()
      .webUserAccountId()
      .title()
      .forename()
      .surname()
      .primaryEmailAddress()
      .telephoneNumber()
      .canLogin()
      .isAccountShared();

  private final UserApi userApi;

  private final EnergyPortalApiWrapper energyPortalApiWrapper;

  @Autowired
  public EnergyPortalUserService(UserApi userApi, EnergyPortalApiWrapper energyPortalApiWrapper) {
    this.userApi = userApi;
    this.energyPortalApiWrapper = energyPortalApiWrapper;
  }

  public List<EnergyPortalUserDto> findUserByUsername(String username) {
    return energyPortalApiWrapper.makeRequest(((logCorrelationId, requestPurpose) ->
        userApi.searchUsersByEmail(
            username,
                USERS_PROJECTION_ROOT,
            requestPurpose.purpose(),
            logCorrelationId.id()
        )
        .stream()
        .filter(User::getCanLogin)
        .map(this::convertToEnergyPortalUser)
        .toList()
    ));
  }

  public List<EnergyPortalUserDto> findByWuaIds(List<WebUserAccountId> webUserAccountIds) {
    return energyPortalApiWrapper.makeRequest(((logCorrelationId, requestPurpose) -> {

      List<Integer> webUserAccountIdApiInputs = webUserAccountIds
          .stream()
          .map(WebUserAccountId::toInt)
          .toList();

      return userApi.searchUsersByIds(
              webUserAccountIdApiInputs,
              USERS_PROJECTION_ROOT,
              requestPurpose.purpose(),
              logCorrelationId.id()
          )
          .stream()
          .map(this::convertToEnergyPortalUser)
          .toList();
    }));
  }

  public Optional<EnergyPortalUserDto> findByWuaId(WebUserAccountId webUserAccountId) {
    return energyPortalApiWrapper.makeRequest(((logCorrelationId, requestPurpose) -> userApi.findUserById(
            webUserAccountId.toInt(),
            USER_PROJECTION_ROOT,
            requestPurpose.purpose(),
            logCorrelationId.id()
        )
        .stream()
        .map(this::convertToEnergyPortalUser)
        .findFirst()
    ));
  }

  private EnergyPortalUserDto convertToEnergyPortalUser(User user) {
    return new EnergyPortalUserDto(
        user.getWebUserAccountId(),
        user.getTitle(),
        user.getForename(),
        user.getSurname(),
        user.getPrimaryEmailAddress(),
        user.getTelephoneNumber(),
        user.getIsAccountShared(),
        user.getCanLogin()
    );
  }
}