package uk.co.nstauthority.scap.audit;

import org.hibernate.envers.RevisionListener;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.authentication.UserDetailService;

@Service
class AuditRevisionEntityListener implements RevisionListener {

  private final UserDetailService userDetailService;

  @Autowired
  AuditRevisionEntityListener(UserDetailService userDetailService) {
    this.userDetailService = userDetailService;
  }

  @Override
  public void newRevision(Object revisionEntity) {
    var auditRevisionEntity = (AuditRevisionEntity) revisionEntity;
    var user = userDetailService.getUserDetail();

    auditRevisionEntity.setWebUserAccountId(user.getWebUserAccountId().id());
  }
}
