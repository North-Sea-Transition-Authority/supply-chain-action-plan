package uk.co.nstauthority.scap.permissionmanagement;


import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;
import uk.co.fivium.energyportal.serviceproviders.epmq.messages.ServiceProviderTeamRolesEpasMessage;
import uk.co.fivium.energyportal.starter.configuration.EnergyPortalAccountsConfigurationProperties;
import uk.co.fivium.energyportal.starter.serviceproviders.EnergyPortalServiceProviderTeamRolesUpdateHandler;
import uk.co.nstauthority.scap.audit.AuditRevisionUtil;
import uk.co.nstauthority.scap.authentication.ServiceUserDetail;
import uk.co.nstauthority.scap.energyportal.EnergyPortalUserService;
import uk.co.nstauthority.scap.energyportal.WebUserAccountId;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberRoleService;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;

@Component
public class TeamRolesUpdateHandler implements EnergyPortalServiceProviderTeamRolesUpdateHandler {

  private static final Logger LOGGER = LoggerFactory.getLogger(TeamRolesUpdateHandler.class);

  private final EnergyPortalUserService energyPortalUserService;
  private final TeamService teamService;
  private final TeamMemberRoleService teamMemberRoleService;
  private final String serviceName;

  TeamRolesUpdateHandler(EnergyPortalUserService energyPortalUserService,
                         TeamService teamService,
                         TeamMemberRoleService teamMemberRoleService,
                         EnergyPortalAccountsConfigurationProperties energyPortalAccountsConfigurationProperties
  ) {
    this.energyPortalUserService = energyPortalUserService;
    this.teamService = teamService;
    this.teamMemberRoleService = teamMemberRoleService;
    this.serviceName = energyPortalAccountsConfigurationProperties.serviceName();
  }

  @Override
  public void accept(ServiceProviderTeamRolesEpasMessage serviceProviderTeamRolesEpasMessage) {
    if (!serviceName.equals(serviceProviderTeamRolesEpasMessage.getService())) {
      return;
    }

    var serviceProviderUserTeamRolesDto = serviceProviderTeamRolesEpasMessage.getServiceProviderUserTeamRolesDto();
    var optionalTeam = teamService.findTeam(TeamId.valueOf(serviceProviderUserTeamRolesDto.teamId()));

    if (optionalTeam.isEmpty()) {
      LOGGER.error("Team not found for id: {}, when updating team_roles from epas team roles update message. correlationId: {}",
          serviceProviderUserTeamRolesDto.teamId(),
          serviceProviderTeamRolesEpasMessage.getCorrelationId()
      );
      return;
    }

    var optionalInvokingUser = energyPortalUserService
        .findByWuaId(
            WebUserAccountId.valueOf(serviceProviderTeamRolesEpasMessage.getDeciderWuaId().toString())
        ).map(ServiceUserDetail::from);

    if (optionalInvokingUser.isEmpty()) {
      LOGGER.error("User not found for id: {}, when updating team_roles from epas team roles update message. correlationId: {}",
          serviceProviderUserTeamRolesDto.wuaId(),
          serviceProviderTeamRolesEpasMessage.getCorrelationId()
      );
      return;
    }

    AuditRevisionUtil.withFallbackAuditUser(
        optionalInvokingUser.get(),
        () -> teamMemberRoleService.updateUserTeamRoles(
            optionalTeam.get(),
            serviceProviderUserTeamRolesDto.wuaId(),
            serviceProviderUserTeamRolesDto.roles()
        )
    );
  }
}