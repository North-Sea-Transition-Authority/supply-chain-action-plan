package uk.co.nstauthority.scap.energyportal;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.tuple;
import static org.junit.jupiter.api.Assertions.assertThrowsExactly;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.List;
import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.fivium.energyportalapi.client.LogCorrelationId;
import uk.co.fivium.energyportalapi.client.RequestPurpose;
import uk.co.fivium.energyportalapi.client.user.UserApi;
import uk.co.nstauthority.scap.branding.CustomerConfigurationProperties;
import uk.co.nstauthority.scap.branding.ServiceConfigurationProperties;
import uk.co.nstauthority.scap.error.exception.EnergyPortalBadRequestException;
import uk.co.nstauthority.scap.error.exception.ScapEntityNotFoundException;

@ExtendWith(MockitoExtension.class)
class EnergyPortalUserServiceTest {

  private static final ServiceConfigurationProperties serviceConfigurationProperties = new ServiceConfigurationProperties(
      "name",
      "mnemonic"
  );

  private static final CustomerConfigurationProperties customerConfigurationProperties = new CustomerConfigurationProperties(
      "name",
      "mnemonic",
      "guidanceDocumentURL"
  );

  @Mock
  UserApi userApi;

  @InjectMocks
  EnergyPortalUserService energyPortalUserService;

  @Test
  void findUserByUsername_whenNoResults_thenEmptyList() {

    var username = "username";

    var userProjectionRoot = EnergyPortalUserService.USERS_PROJECTION_ROOT;

    when(userApi.searchUsersByEmail(
        eq(username),
        eq(userProjectionRoot),
        any(RequestPurpose.class),
        any(LogCorrelationId.class)
    )).thenReturn(Collections.emptyList());

    assertTrue(energyPortalUserService.findUsersByUsername(username).isEmpty());
  }

  @Test
  void findUserByUsername_whenUserFoundAndCanLogIn_thenPopulatedListCorrectlyMapped() {

    var username = "username";
    var expectedUser = EpaUserTestUtil.Builder()
        .canLogin(true)
        .build();

    var userProjectionRoot = EnergyPortalUserService.USERS_PROJECTION_ROOT;

    when(userApi.searchUsersByEmail(
        eq(username),
        eq(userProjectionRoot),
        any(RequestPurpose.class),
        any(LogCorrelationId.class)
    )).thenReturn(List.of(expectedUser));

    assertThat(energyPortalUserService.findUsersByUsername(username))
        .extracting(
            EnergyPortalUserDto::webUserAccountId,
            EnergyPortalUserDto::title,
            EnergyPortalUserDto::forename,
            EnergyPortalUserDto::surname,
            EnergyPortalUserDto::emailAddress,
            EnergyPortalUserDto::telephoneNumber,
            EnergyPortalUserDto::isSharedAccount,
            EnergyPortalUserDto::canLogin
        )
        .containsExactly(
            tuple(
                Long.valueOf(expectedUser.getWebUserAccountId()),
                expectedUser.getTitle(),
                expectedUser.getForename(),
                expectedUser.getSurname(),
                expectedUser.getPrimaryEmailAddress(),
                expectedUser.getTelephoneNumber(),
                expectedUser.getIsAccountShared(),
                expectedUser.getCanLogin()
            )
        );
  }

  @Test
  void findUserByUsername_whenUsersFound_thenOnlyThoseWithCanLoginTrueReturned() {

    var username = "username";

    var canLoginUser = EpaUserTestUtil.Builder()
        .canLogin(true)
        .withWebUserAccountId(100)
        .build();

    var notLoginUser = EpaUserTestUtil.Builder()
        .canLogin(false)
        .withWebUserAccountId(200)
        .build();

    var userProjectionRoot = EnergyPortalUserService.USERS_PROJECTION_ROOT;

    when(userApi.searchUsersByEmail(
        eq(username),
        eq(userProjectionRoot),
        any(RequestPurpose.class),
        any(LogCorrelationId.class)
    )).thenReturn(List.of(
        canLoginUser,
        notLoginUser
    ));

    assertThat(energyPortalUserService.findUsersByUsername(username))
        .extracting(EnergyPortalUserDto::webUserAccountId)
        .containsExactly(Long.valueOf(canLoginUser.getWebUserAccountId()));
  }

  @Test
  void findByWuaIds_whenNoResults_thenEmptyList() {

    var webUserAccountId = new WebUserAccountId(123L);

    var userProjectionRoot = EnergyPortalUserService.USERS_PROJECTION_ROOT;

    when(userApi.searchUsersByIds(
        eq(List.of(webUserAccountId.toInt())),
        eq(userProjectionRoot),
        any(RequestPurpose.class),
        any(LogCorrelationId.class)
    )).thenReturn(Collections.emptyList());

    assertTrue(energyPortalUserService.findByWuaIds(List.of(webUserAccountId)).isEmpty());
  }

  @Test
  void findByWuaIds_whenResults_thenPopulatedListCorrectlyMapped() {

    var webUserAccountId = new WebUserAccountId(123L);
    var expectedUser = EpaUserTestUtil.Builder().build();

    var userProjectionRoot = EnergyPortalUserService.USERS_PROJECTION_ROOT;

      when(userApi.searchUsersByIds(
          eq(List.of(webUserAccountId.toInt())),
          eq(userProjectionRoot),
          any(RequestPurpose.class),
          any(LogCorrelationId.class)
      )).thenReturn(List.of(expectedUser));

    assertThat(energyPortalUserService.findByWuaIds(List.of(webUserAccountId)))
        .extracting(
            EnergyPortalUserDto::webUserAccountId,
            EnergyPortalUserDto::title,
            EnergyPortalUserDto::forename,
            EnergyPortalUserDto::surname,
            EnergyPortalUserDto::emailAddress,
            EnergyPortalUserDto::telephoneNumber,
            EnergyPortalUserDto::isSharedAccount,
            EnergyPortalUserDto::canLogin
        )
        .containsExactly(
            tuple(
                Long.valueOf(expectedUser.getWebUserAccountId()),
                expectedUser.getTitle(),
                expectedUser.getForename(),
                expectedUser.getSurname(),
                expectedUser.getPrimaryEmailAddress(),
                expectedUser.getTelephoneNumber(),
                expectedUser.getIsAccountShared(),
                expectedUser.getCanLogin()
            )
        );
  }

  @Test
  void findByWuaId_whenFound_thenPopulatedOptional() {

    var expectedUser = EpaUserTestUtil.Builder().build();
    var webUserAccountId = new WebUserAccountId(Long.valueOf(expectedUser.getWebUserAccountId()));

    var userProjectionRoot = EnergyPortalUserService.USER_PROJECTION_ROOT;

    when(userApi.findUserById(
        eq(webUserAccountId.toInt()),
        eq(userProjectionRoot),
        any(RequestPurpose.class),
        any(LogCorrelationId.class)
    )).thenReturn(Optional.of(expectedUser));

    var resultingUser = energyPortalUserService.findByWuaId(webUserAccountId);

    assertThat(resultingUser).isPresent();
    assertThat(resultingUser.get())
        .extracting(
            EnergyPortalUserDto::webUserAccountId,
            EnergyPortalUserDto::title,
            EnergyPortalUserDto::forename,
            EnergyPortalUserDto::surname,
            EnergyPortalUserDto::emailAddress,
            EnergyPortalUserDto::telephoneNumber,
            EnergyPortalUserDto::isSharedAccount,
            EnergyPortalUserDto::canLogin
        )
        .containsExactly(
            Long.valueOf(expectedUser.getWebUserAccountId()),
            expectedUser.getTitle(),
            expectedUser.getForename(),
            expectedUser.getSurname(),
            expectedUser.getPrimaryEmailAddress(),
            expectedUser.getTelephoneNumber(),
            expectedUser.getIsAccountShared(),
            expectedUser.getCanLogin()
        );
  }

  @Test
  void findByWuaId_whenNotFound_thenEmptyOptional() {

    var webUserAccountId = new WebUserAccountId(123L);

    var userProjectionRoot = EnergyPortalUserService.USER_PROJECTION_ROOT;

    when(userApi.findUserById(
        eq(webUserAccountId.toInt()),
        eq(userProjectionRoot),
        any(RequestPurpose.class),
        any(LogCorrelationId.class)
    )).thenReturn(Optional.empty());

    assertThat(energyPortalUserService.findByWuaId(webUserAccountId)).isEmpty();
  }

  @Test
  void getEnergyPortalUser_whenNotFound_thenScapException() {
    var webUserAccountId = new WebUserAccountId(123L);
    var userProjectionRoot = EnergyPortalUserService.USER_PROJECTION_ROOT;

    when(userApi.findUserById(
        eq(webUserAccountId.toInt()),
        eq(userProjectionRoot),
        any(RequestPurpose.class),
        any(LogCorrelationId.class)
    )).thenReturn(Optional.empty());

    assertThrowsExactly(ScapEntityNotFoundException.class,
        () -> energyPortalUserService.getEnergyPortalUser(webUserAccountId));
  }

  @Test
  void getEnergyPortalUser_whenSharedAccount_thenEPAException() {
    var webUserAccountId = new WebUserAccountId(123L);
    var userProjectionRoot = EnergyPortalUserService.USER_PROJECTION_ROOT;
    var expectedUser = EpaUserTestUtil.Builder()
        .isSharedAccount(true)
        .build();

    when(userApi.findUserById(
        eq(webUserAccountId.toInt()),
        eq(userProjectionRoot),
        any(RequestPurpose.class),
        any(LogCorrelationId.class)
    )).thenReturn(Optional.of(expectedUser));

    assertThrowsExactly(EnergyPortalBadRequestException.class,
        () -> energyPortalUserService.getEnergyPortalUser(webUserAccountId));
  }

  @Test
  void getEnergyPortalUser_whenDisabled_thenEPAException() {
    var webUserAccountId = new WebUserAccountId(123L);
    var userProjectionRoot = EnergyPortalUserService.USER_PROJECTION_ROOT;
    var expectedUser = EpaUserTestUtil.Builder()
        .canLogin(false)
        .build();

    when(userApi.findUserById(
        eq(webUserAccountId.toInt()),
        eq(userProjectionRoot),
        any(RequestPurpose.class),
        any(LogCorrelationId.class)
    )).thenReturn(Optional.of(expectedUser));

    assertThrowsExactly(EnergyPortalBadRequestException.class,
        () -> energyPortalUserService.getEnergyPortalUser(webUserAccountId));
  }

  @Test
  void getEnergyPortalUser_whenSharedAccount_thenScapException() {
    var webUserAccountId = new WebUserAccountId(123L);
    var userProjectionRoot = EnergyPortalUserService.USER_PROJECTION_ROOT;
    var expectedUser = EpaUserTestUtil.Builder()
        .build();

    when(userApi.findUserById(
        eq(webUserAccountId.toInt()),
        eq(userProjectionRoot),
        any(RequestPurpose.class),
        any(LogCorrelationId.class)
    )).thenReturn(Optional.of(expectedUser));

   assertThat(energyPortalUserService.getEnergyPortalUser(webUserAccountId).forename())
       .isEqualTo(expectedUser.getForename());
  }
}