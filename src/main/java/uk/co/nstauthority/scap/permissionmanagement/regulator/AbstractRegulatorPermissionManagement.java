package uk.co.nstauthority.scap.permissionmanagement.regulator;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import uk.co.nstauthority.scap.permissionmanagement.Team;
import uk.co.nstauthority.scap.permissionmanagement.TeamId;

@Component
abstract class AbstractRegulatorPermissionManagement {

  private final RegulatorTeamService regulatorTeamService;

  @Autowired
  AbstractRegulatorPermissionManagement(RegulatorTeamService regulatorTeamService) {
    this.regulatorTeamService = regulatorTeamService;
  }

  Team getRegulatorTeam(TeamId teamId) {
    return regulatorTeamService.getTeamOrThrow(teamId);
  }
}