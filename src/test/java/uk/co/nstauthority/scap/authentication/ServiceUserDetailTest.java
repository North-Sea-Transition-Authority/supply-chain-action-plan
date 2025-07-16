package uk.co.nstauthority.scap.authentication;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

class ServiceUserDetailTest {

  @Test
  void displayName_whenProxyUser_thenIncludeProxyUsername() {
    var forename = "forename";
    var surname = "surname";
    var proxyWuaId = 999L;
    var proxyUserName = "proxyUsername";

    var serviceUserDetail = ServiceUserDetailTestUtil.Builder()
        .withForename(forename)
        .withSurname(surname)
        .withProxyUserDisplayName(proxyUserName)
        .withProxyWuaId(proxyWuaId)
        .build();

    var result = serviceUserDetail.displayName();

    assertThat(result).isEqualTo(String.format("%s as %s %s", proxyUserName, forename, surname));
  }

  @Test
  void displayNameIncludingAnyProxyUser_whenNotProxyUser_theJustUsersName() {
    var forename = "forename";
    var surname = "surname";

    var serviceUserDetail = ServiceUserDetailTestUtil.Builder()
        .withForename(forename)
        .withSurname(surname)
        .withProxyUserDisplayName(null)
        .withProxyWuaId(null)
        .build();

    var result = serviceUserDetail.displayName();

    assertThat(result).isEqualTo(String.format("%s %s", forename, surname));
  }
}