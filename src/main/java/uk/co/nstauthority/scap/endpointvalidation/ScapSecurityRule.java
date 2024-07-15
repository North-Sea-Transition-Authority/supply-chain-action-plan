package uk.co.nstauthority.scap.endpointvalidation;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.lang.annotation.Annotation;
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
