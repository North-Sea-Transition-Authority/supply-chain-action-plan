package uk.co.nstauthority.scap.energyportal.user;

import java.util.List;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.energyportal.organisationgroup.OrganisationGroupDto;
import uk.co.nstauthority.scap.energyportal.organisationgroup.OrganisationGroupQueryService;
import uk.co.nstauthority.scap.permissionmanagement.Team;
import uk.co.nstauthority.scap.permissionmanagement.TeamType;

@Service
public class AllowedDomainService {

  private final OrganisationGroupQueryService organisationGroupQueryService;

  AllowedDomainService(OrganisationGroupQueryService organisationGroupQueryService) {
    this.organisationGroupQueryService = organisationGroupQueryService;
  }

  public boolean isAllowedDomain(String userEmail, Team team) {
    var group = switch (team.getTeamType()) {
      case TeamType.INDUSTRY -> organisationGroupQueryService
          .getOrganisationGroupById(team.getEnergyPortalOrgGroupId());
      case TeamType.REGULATOR -> organisationGroupQueryService.getRegulatorOrganisationGroup();
    };

    var lowerEmail = userEmail.toLowerCase();
    return group.map(OrganisationGroupDto::emailDomains).orElse(List.of()).stream()
        .map(String::toLowerCase)
        .anyMatch(domain -> lowerEmail.endsWith("@" + domain));
  }
}