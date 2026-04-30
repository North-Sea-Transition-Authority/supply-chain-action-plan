package uk.co.nstauthority.scap.energyportal;

import java.util.List;
import java.util.Optional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
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

  public List<User> searchUsersByEmail(String email) {
    return userApi.searchUsersByEmail(
        email,
        USERS_PROJECTION_ROOT,
        FIND_USERS_REQUEST_PURPOSE
    );
  }

  public Optional<EnergyPortalUserDto> findUserByEmail(String email) {
    return searchUsersByEmail(email).stream()
        .filter(User::getCanLogin)
        .map(this::convertToEnergyPortalUser)
        .findFirst();
  }

  public List<User> searchUsersByIds(List<WebUserAccountId> webUserAccountIds) {
    return userApi.searchUsersByIds(
        webUserAccountIds.stream().map(WebUserAccountId::id).toList(),
        USERS_PROJECTION_ROOT,
        FIND_USERS_REQUEST_PURPOSE
    );
  }

  public List<EnergyPortalUserDto> findByWuaIds(List<WebUserAccountId> webUserAccountIds) {
    return searchUsersByIds(webUserAccountIds).stream()
        .map(this::convertToEnergyPortalUser)
        .toList();
  }

  public Optional<User> findUserById(WebUserAccountId webUserAccountId) {
    return userApi.findUserById(
        webUserAccountId.id(),
        USER_PROJECTION_ROOT,
        FIND_USER_REQUEST_PURPOSE
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
        user.getCanLogin()
    );
  }
}
