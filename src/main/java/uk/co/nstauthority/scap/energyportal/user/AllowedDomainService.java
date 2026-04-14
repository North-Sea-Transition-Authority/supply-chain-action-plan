package uk.co.nstauthority.scap.energyportal.user;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
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
    Optional<OrganisationGroupDto> group;
    switch (team.getTeamType()) {
      case TeamType.INDUSTRY -> group = organisationGroupQueryService
          .getOrganisationGroupById(team.getEnergyPortalOrgGroupId());
      case TeamType.REGULATOR -> group = organisationGroupQueryService.getRegulatorOrganisationGroup();
      default -> throw new IllegalStateException("Unexpected value: " + team.getTeamType());
    }

    List<String> emailDomains = group.map(OrganisationGroupDto::emailDomains).orElseGet(ArrayList::new);
    return emailDomains.stream()
        .map(String::toLowerCase)
        .anyMatch(domain -> userEmail.toLowerCase().endsWith('@' + domain));
  }
}