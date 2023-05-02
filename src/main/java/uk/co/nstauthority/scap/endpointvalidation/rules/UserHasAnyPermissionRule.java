package uk.co.nstauthority.scap.endpointvalidation.rules;

import java.lang.annotation.Annotation;
import java.util.Arrays;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import uk.co.nstauthority.scap.authentication.ServiceUserDetail;
import uk.co.nstauthority.scap.endpointvalidation.ScapSecurityRule;
import uk.co.nstauthority.scap.endpointvalidation.SecurityRuleResult;
import uk.co.nstauthority.scap.endpointvalidation.annotations.UserHasAnyPermission;
import uk.co.nstauthority.scap.permissionmanagement.Team;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberService;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;

public class UserHasAnyPermissionRule implements ScapSecurityRule {
  private static final Logger LOGGER = LoggerFactory.getLogger(UserHasAnyPermissionRule.class);
  private final TeamMemberService teamMemberService;

  @Autowired
  public UserHasAnyPermissionRule(TeamMemberService teamMemberService) {
    this.teamMemberService = teamMemberService;
  }

  @Override
  public Class<? extends Annotation> supports() {
    return UserHasAnyPermission.class;
  }

  @Override
  public SecurityRuleResult check(Object annotation,
                                  HttpServletRequest request,
                                  HttpServletResponse response,
                                  ServiceUserDetail userDetail,
                                  ScapDetail scapDetail,
                                  Team team) {

    var userPermissions = teamMemberService.getAllPermissionsForUser(userDetail);
    var requiredPermissions = (UserHasAnyPermission) annotation;
    var hasPermission = Arrays.stream(requiredPermissions.permissions())
        .anyMatch(userPermissions::contains);

    if (hasPermission) {
      return SecurityRuleResult.continueAsNormal();
    }
    LOGGER.warn("User with ID: %s does not have the required permissions.".formatted(userDetail.wuaId()));
    return SecurityRuleResult.checkFailedWithStatus(HttpStatus.FORBIDDEN);
  }
}
