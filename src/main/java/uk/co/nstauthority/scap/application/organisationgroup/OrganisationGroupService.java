package uk.co.nstauthority.scap.application.organisationgroup;

import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.fivium.energyportalapi.client.organisation.OrganisationApi;
import uk.co.fivium.energyportalapi.generated.client.OrganisationGroupByIdProjectionRoot;
import uk.co.fivium.energyportalapi.generated.client.OrganisationGroupsByNameProjectionRoot;
import uk.co.fivium.energyportalapi.generated.types.OrganisationGroup;
import uk.co.nstauthority.scap.fds.searchselector.RestSearchResult;
import uk.co.nstauthority.scap.fds.searchselector.RestSearchSingleResult;

@Service
public class OrganisationGroupService {
  public final OrganisationApi organisationApi;

  @Autowired
  public OrganisationGroupService(OrganisationApi organisationApi) {
    this.organisationApi = organisationApi;
  }

  public List<OrganisationGroup> getOrganisationGroupsByName(String name, String purpose) {
    var organisationGroupFilter = new OrganisationGroupsByNameProjectionRoot()
        .organisationGroupId()
        .name();

    return organisationApi.searchOrganisationGroupsByName(name, organisationGroupFilter, purpose);
  }

  public RestSearchResult organisationGroupsToSearchResult(List<OrganisationGroup> queryResults) {
    return new RestSearchResult(queryResults.stream()
        .map(organisationGroup -> new RestSearchSingleResult(
            organisationGroup.getOrganisationGroupId().toString(),
            organisationGroup.getName()))
        .collect(Collectors.toList()));
  }

  public Optional<OrganisationGroup> getOrganisationGroupById(Integer id, String purpose) {
    var organisationGroupFilter = new OrganisationGroupByIdProjectionRoot()
        .organisationGroupId()
        .name();

    return organisationApi.searchOrganisationGroupById(id, organisationGroupFilter, purpose);
  }
}
