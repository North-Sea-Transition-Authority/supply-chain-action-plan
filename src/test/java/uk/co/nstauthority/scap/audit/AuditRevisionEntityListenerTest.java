package uk.co.nstauthority.scap.audit;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.authentication.TestUserProvider;
import uk.co.nstauthority.scap.authentication.UserDetailService;

@ExtendWith(MockitoExtension.class)
class AuditRevisionEntityListenerTest {

  @Mock
  UserDetailService userDetailService;

  @InjectMocks
  AuditRevisionEntityListener auditRevisionEntityListener;

  @Test
  void newRevision() {
    var revisionEntity = new AuditRevisionEntity();
    var user = TestUserProvider.getUser();

    when(userDetailService.getUserDetail()).thenReturn(user);

    auditRevisionEntityListener.newRevision(revisionEntity);

    assertThat(revisionEntity.getWebUserAccountId()).isEqualTo(user.getWebUserAccountId().id());
  }
}