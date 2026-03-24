package uk.co.nstauthority.scap.energyportal.organisationgroup;

import java.util.Optional;
import org.springframework.stereotype.Service;
import uk.co.fivium.energyportal.starter.configuration.WellKnownOrganisationGroupsConfigurationProperties;
import uk.co.fivium.energyportalapi.client.RequestPurpose;
import uk.co.fivium.energyportalapi.client.organisation.OrganisationApi;
import uk.co.fivium.energyportalapi.generated.client.OrganisationGroupProjectionRoot;

@Service
public class OrganisationGroupQueryService {
  public static final OrganisationGroupProjectionRoot ORGANISATION_GROUP_PROJECTION_ROOT =
      new OrganisationGroupProjectionRoot()
          .organisationGroupId()
          .name()
          .emailDomains()
          .domain()
          .root();

  private final OrganisationApi organisationApi;
  private final WellKnownOrganisationGroupsConfigurationProperties wellKnownOrganisationGroups;

  public OrganisationGroupQueryService(
      OrganisationApi organisationApi,
      WellKnownOrganisationGroupsConfigurationProperties wellKnownOrganisationGroups
  ) {
    this.organisationApi = organisationApi;
    this.wellKnownOrganisationGroups = wellKnownOrganisationGroups;
  }

  public Optional<OrganisationGroupDto> getOrganisationGroupById(Integer id) {
    return organisationApi.findOrganisationGroup(
            id,
            ORGANISATION_GROUP_PROJECTION_ROOT,
            new RequestPurpose("getOrganisationGroupById")
        )
        .map(OrganisationGroupDto::from);
  }

  public Optional<OrganisationGroupDto> getRegulatorOrganisationGroup() {
    return getOrganisationGroupById(wellKnownOrganisationGroups.nsta().idAsInteger());
  }
}