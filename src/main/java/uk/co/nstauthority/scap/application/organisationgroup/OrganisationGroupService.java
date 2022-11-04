package uk.co.nstauthority.scap.application.organisationgroup;

import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.fivium.energyportalapi.client.LogCorrelationId;
import uk.co.fivium.energyportalapi.client.RequestPurpose;
import uk.co.fivium.energyportalapi.client.organisation.OrganisationApi;
import uk.co.fivium.energyportalapi.generated.client.OrganisationGroupProjectionRoot;
import uk.co.fivium.energyportalapi.generated.client.OrganisationGroupsProjectionRoot;
import uk.co.fivium.energyportalapi.generated.types.OrganisationGroup;
import uk.co.nstauthority.scap.fds.searchselector.RestSearchItem;
import uk.co.nstauthority.scap.fds.searchselector.RestSearchResult;

@Service
public class OrganisationGroupService {
  private final OrganisationApi organisationApi;

  @Autowired
  public OrganisationGroupService(OrganisationApi organisationApi) {
    this.organisationApi = organisationApi;
  }

  public List<OrganisationGroup> getOrganisationGroupsByName(String name, String purpose) {
    var organisationGroupFilter = new OrganisationGroupsProjectionRoot()
        .organisationGroupId()
        .name();
    var requestPurpose = new RequestPurpose(purpose);

    return organisationApi.searchOrganisationGroups(name, organisationGroupFilter, requestPurpose, getLogCorrelationId());
  }

  public RestSearchResult organisationGroupsToSearchResult(List<OrganisationGroup> queryResults) {
    return new RestSearchResult(queryResults.stream()
        .map(organisationGroup -> new RestSearchItem(
            organisationGroup.getOrganisationGroupId().toString(),
            organisationGroup.getName()))
        .collect(Collectors.toList()));
  }

  public Optional<OrganisationGroup> getOrganisationGroupById(Integer id, String purpose) {
    var organisationGroupFilter = new OrganisationGroupProjectionRoot()
        .organisationGroupId()
        .name();
    var requestPurpose = new RequestPurpose(purpose);

    return organisationApi.findOrganisationGroup(id, organisationGroupFilter, requestPurpose, getLogCorrelationId());
  }

  private LogCorrelationId getLogCorrelationId() {
    return new LogCorrelationId(String.valueOf(UUID.randomUUID()));
  }
}
