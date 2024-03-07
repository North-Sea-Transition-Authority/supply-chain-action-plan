package uk.co.nstauthority.scap.endpointvalidation;

import java.lang.annotation.Annotation;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import uk.co.nstauthority.scap.authentication.ServiceUserDetail;
import uk.co.nstauthority.scap.permissionmanagement.Team;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;

public interface ScapSecurityRule {

  Class<? extends Annotation> supports();

  SecurityRuleResult check(Object annotation,
                           HttpServletRequest request,
                           HttpServletResponse response,
                           ServiceUserDetail userDetail,
                           ScapDetail scapDetail,
                           Team team);
}
