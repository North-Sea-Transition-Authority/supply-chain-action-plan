package uk.co.nstauthority.scap.authentication;


import uk.co.nstauthority.scap.error.exception.IllegalUtilClassInstantiationException;

public class ServiceUserDetailTestUtil {

  private ServiceUserDetailTestUtil() {
    throw new IllegalUtilClassInstantiationException(this.getClass());
  }

  public static Builder Builder() {
    return new Builder();
  }

  public static class Builder {

    private Builder() {}

    private Long wuaId = 1L;
    private Long personId = 2L;
    private String forename = "Forename";
    private String surname = "Surname";
    private String emailAddress = "test.user@test.com";
    private Long proxyWuaId = null;
    private String proxyUserDisplayName = "Proxy User";

    public ServiceUserDetail build() {
      return new ServiceUserDetail(wuaId, personId, forename, surname, emailAddress, proxyWuaId, proxyUserDisplayName);
    }

    public Builder withWuaId(Long wuaId) {
      this.wuaId = wuaId;
      return this;
    }

    public Builder withPersonId(Long personId) {
      this.personId = personId;
      return this;
    }

    public Builder withForename(String forename) {
      this.forename = forename;
      return this;
    }

    public Builder withSurname(String surname) {
      this.surname = surname;
      return this;
    }

    public Builder withEmailAddress(String emailAddress) {
      this.emailAddress = emailAddress;
      return this;
    }

    public Builder withProxyWuaId(Long proxyWuaId) {
      this.proxyWuaId = proxyWuaId;
      return this;
    }

    public Builder withProxyUserDisplayName(String proxyUserDisplayName) {
      this.proxyUserDisplayName = proxyUserDisplayName;
      return this;
    }
  }
}
