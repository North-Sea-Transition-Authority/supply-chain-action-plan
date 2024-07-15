package uk.co.nstauthority.scap.audit;

import java.util.Optional;
import org.hibernate.envers.RevisionListener;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.authentication.ServiceSaml2Authentication;
import uk.co.nstauthority.scap.authentication.ServiceUserDetail;

@Service
class AuditRevisionEntityListener implements RevisionListener {

  @Override
  public void newRevision(Object revision) {
    var auditRevisionEntity = (AuditRevisionEntity) revision;
    getUserDetail().ifPresent(user -> auditRevisionEntity.setWebUserAccountId(user.getWebUserAccountId().id()));
  }

  // We use this method and not UserDetailService#getUserDetail as that method throws an exception if there is no user
  // logged in and there are scenarios when audit revisions are created when there is no user logged in.
  // Examples are:
  // - When an email is sent by the notification library.
  // This method is located here and not in the UserDetailService as there are no other places that should be calling
  // getUserDetail when there is no user logged in.
  private Optional<ServiceUserDetail> getUserDetail() {
    if (SecurityContextHolder.getContext().getAuthentication() instanceof ServiceSaml2Authentication authentication
        && authentication.getPrincipal() instanceof ServiceUserDetail serviceUserDetail) {
      return Optional.of(serviceUserDetail);
    }

    return Optional.empty();
  }
}
