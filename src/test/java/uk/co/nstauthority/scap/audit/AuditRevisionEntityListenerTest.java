package uk.co.nstauthority.scap.audit;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Set;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.context.SecurityContextImpl;
import uk.co.nstauthority.scap.authentication.SamlAuthenticationUtil;
import uk.co.nstauthority.scap.authentication.ServiceSaml2Authentication;
import uk.co.nstauthority.scap.authentication.ServiceUserDetailTestUtil;

@ExtendWith(MockitoExtension.class)
class AuditRevisionEntityListenerTest {

  private final AuditRevisionEntityListener auditRevisionEntityListener = new AuditRevisionEntityListener();

  @AfterAll
  static void tearDown() {
    SecurityContextHolder.setContext(new SecurityContextImpl(null));
  }

  @Test
  void newRevision_userNoProxyInContext() {
    var serviceUserDetail = ServiceUserDetailTestUtil.Builder()
        .withWuaId(100L)
        .withProxyWuaId(null)
        .build();

    SamlAuthenticationUtil.Builder()
        .withUser(serviceUserDetail)
        .setSecurityContext();

    var auditRevisionEntity = new AuditRevisionEntity();

    auditRevisionEntityListener.newRevision(auditRevisionEntity);

    assertThat(auditRevisionEntity.getWebUserAccountId()).isEqualTo(100L);
    assertThat(auditRevisionEntity.getProxyWebUserAccountId()).isNull();
  }

  @Test
  void newRevision_proxyUserInContext() {

    var serviceUserDetail = ServiceUserDetailTestUtil.Builder()
        .withWuaId(100L)
        .withProxyWuaId(200L)
        .build();

    SamlAuthenticationUtil.Builder()
        .withUser(serviceUserDetail)
        .setSecurityContext();

    var auditRevisionEntity = new AuditRevisionEntity();

    auditRevisionEntityListener.newRevision(auditRevisionEntity);

    assertThat(auditRevisionEntity.getWebUserAccountId()).isEqualTo(100L);
    assertThat(auditRevisionEntity.getProxyWebUserAccountId()).isEqualTo(200L);
  }

  @Test
  void newRevision_noPrincipal() {
    SecurityContextHolder.setContext(new SecurityContextImpl(new ServiceSaml2Authentication(null, Set.of())));

    var auditRevisionEntity = new AuditRevisionEntity();

    auditRevisionEntityListener.newRevision(auditRevisionEntity);

    assertThat(auditRevisionEntity.getWebUserAccountId()).isNull();
  }

  @Test
  void newRevision_noAuthenticationInContext() {
    SecurityContextHolder.setContext(new SecurityContextImpl(null));

    var auditRevisionEntity = new AuditRevisionEntity();

    auditRevisionEntityListener.newRevision(auditRevisionEntity);

    assertThat(auditRevisionEntity.getWebUserAccountId()).isNull();
  }

  @Test
  void newRevision_whenNoAuthenticatedUser_thenUseFallbackUser() {
    SecurityContextHolder.setContext(new SecurityContextImpl(null));

    var auditRevision = new AuditRevisionEntity();
    var serviceUserDetail = ServiceUserDetailTestUtil.Builder().build();

    try (MockedStatic<AuditRevisionUtil> mockedUtil = Mockito.mockStatic(AuditRevisionUtil.class)) {
      mockedUtil.when(AuditRevisionUtil::getFallbackAuditUser)
          .thenReturn(serviceUserDetail);

      auditRevisionEntityListener.newRevision(auditRevision);
    }

    assertThat(auditRevision.getWebUserAccountId()).isEqualTo(serviceUserDetail.wuaId());
    assertThat(auditRevision.getProxyWebUserAccountId()).isEqualTo(serviceUserDetail.proxyWuaId());
  }
}