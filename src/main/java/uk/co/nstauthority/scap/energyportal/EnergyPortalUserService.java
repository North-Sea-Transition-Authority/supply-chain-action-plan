package uk.co.nstauthority.scap.energyportal;

import java.util.List;
import java.util.Optional;
import java.util.UUID;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.fivium.energyportalapi.client.LogCorrelationId;
import uk.co.fivium.energyportalapi.client.RequestPurpose;
import uk.co.fivium.energyportalapi.client.user.UserApi;
import uk.co.fivium.energyportalapi.generated.client.UserProjectionRoot;
import uk.co.fivium.energyportalapi.generated.client.UsersProjectionRoot;
import uk.co.fivium.energyportalapi.generated.types.User;
import uk.co.nstauthority.scap.error.exception.EnergyPortalBadRequestException;
import uk.co.nstauthority.scap.error.exception.ScapEntityNotFoundException;

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

  static final RequestPurpose FIND_USERS_REQUEST_PURPOSE = new RequestPurpose("test");
  static final RequestPurpose FIND_USER_REQUEST_PURPOSE = new RequestPurpose("test");

  private final UserApi userApi;

  @Autowired
  public EnergyPortalUserService(UserApi userApi) {
    this.userApi = userApi;
  }

  public List<User> searchUsersByUsername(String username) {
    return userApi.searchUsersByEmail(
        username,
        USERS_PROJECTION_ROOT,
        FIND_USERS_REQUEST_PURPOSE,
        new LogCorrelationId(UUID.randomUUID().toString())
    );
  }

  public List<EnergyPortalUserDto> findUsersByUsername(String username) {
    return searchUsersByUsername(username).stream()
        .filter(User::getCanLogin)
        .map(this::convertToEnergyPortalUser)
        .toList();
  }

  public List<User> searchUsersByIds(List<WebUserAccountId> webUserAccountIds) {
    var userIds = webUserAccountIds.stream()
        .map(WebUserAccountId::toInt)
        .toList();

    return userApi.searchUsersByIds(
        userIds,
        USERS_PROJECTION_ROOT,
        FIND_USERS_REQUEST_PURPOSE,
        new LogCorrelationId(UUID.randomUUID().toString())
    );
  }

  public List<EnergyPortalUserDto> findByWuaIds(List<WebUserAccountId> webUserAccountIds) {
    return searchUsersByIds(webUserAccountIds).stream()
        .map(this::convertToEnergyPortalUser)
        .toList();
  }

  public Optional<User> findUserById(WebUserAccountId webUserAccountId) {
    return userApi.findUserById(
        webUserAccountId.toInt(),
        USER_PROJECTION_ROOT,
        FIND_USER_REQUEST_PURPOSE,
        new LogCorrelationId(UUID.randomUUID().toString())
    );
  }

  public Optional<EnergyPortalUserDto> findByWuaId(WebUserAccountId webUserAccountId) {
    return findUserById(webUserAccountId)
        .stream()
        .map(this::convertToEnergyPortalUser)
        .findFirst();
  }

  public EnergyPortalUserDto getEnergyPortalUser(WebUserAccountId webUserAccountId) {
    var energyPortalUser = findByWuaId(webUserAccountId)
        .orElseThrow(() -> new ScapEntityNotFoundException(
            "No Energy Portal user with WUA_ID: %s could be found".formatted(webUserAccountId)
        ));

    if (energyPortalUser.isSharedAccount()) {
      throw new EnergyPortalBadRequestException(
          "Energy Portal user with WUA_ID: %s is a shared account and is not allowed to be added to this service"
              .formatted(webUserAccountId)
      );
    }

    if (!energyPortalUser.canLogin()) {
      throw new EnergyPortalBadRequestException(
          ("Energy Portal user with WUA_ID: %s does not have login access to the Energy Portal " +
              "and is not allowed to be added to this service")
              .formatted(webUserAccountId)
      );
    }

    return energyPortalUser;
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