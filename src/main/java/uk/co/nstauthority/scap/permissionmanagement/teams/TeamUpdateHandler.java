package uk.co.nstauthority.scap.permissionmanagement.teams;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;
import uk.co.fivium.energyportal.starter.organisationgroup.EnergyPortalOrganisationGroupConsumer;
import uk.co.fivium.energyportal.starter.organisationgroup.EnergyPortalOrganisationGroupEvent;

@Component
class TeamUpdateHandler implements EnergyPortalOrganisationGroupConsumer {
  private static final Logger LOGGER = LoggerFactory.getLogger(TeamUpdateHandler.class);

  private final TeamService teamService;

  TeamUpdateHandler(TeamService teamService) {
    this.teamService = teamService;
  }

  @Override
  public void onEnergyPortalOrganisationGroupEvent(EnergyPortalOrganisationGroupEvent energyPortalOrganisationGroupEvent) {
    if (energyPortalOrganisationGroupEvent.isCreated()) {
      LOGGER.info("Received organisation group created event for group {}", energyPortalOrganisationGroupEvent.groupId());
      return;
    }

    var teamOptional = teamService.findByEnergyPortalOrgGroupId(Math.toIntExact(energyPortalOrganisationGroupEvent.groupId()));

    if (teamOptional.isEmpty() || teamOptional.get().getDisplayName().equals(energyPortalOrganisationGroupEvent.name())) {
      return;
    }

    teamService.updateTeamName(teamOptional.get(), energyPortalOrganisationGroupEvent.name());
    LOGGER.info("Updated team name for group {}", energyPortalOrganisationGroupEvent.groupId());
  }
}
