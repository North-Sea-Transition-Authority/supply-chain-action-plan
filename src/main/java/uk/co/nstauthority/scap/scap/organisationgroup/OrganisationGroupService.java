package uk.co.nstauthority.scap.scap.organisationgroup;

import com.google.common.annotations.VisibleForTesting;
import java.util.List;
import java.util.Optional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.fivium.energyportalapi.client.RequestPurpose;
import uk.co.fivium.energyportalapi.client.organisation.OrganisationApi;
import uk.co.fivium.energyportalapi.generated.client.OrganisationGroupProjectionRoot;
import uk.co.fivium.energyportalapi.generated.client.OrganisationGroupsProjectionRoot;
import uk.co.fivium.energyportalapi.generated.types.OrganisationGroup;
import uk.co.nstauthority.scap.fds.searchselector.RestSearchItem;
import uk.co.nstauthority.scap.fds.searchselector.RestSearchResult;

@Service
public class OrganisationGroupService {

  @VisibleForTesting
  static final OrganisationGroupsProjectionRoot ORGANISATION_GROUPS_PROJECTION_ROOT =
      new OrganisationGroupsProjectionRoot().organisationGroupId().name();

  private final OrganisationApi organisationApi;

  @Autowired
  public OrganisationGroupService(OrganisationApi organisationApi) {
    this.organisationApi = organisationApi;
  }

  public List<OrganisationGroup> getOrganisationGroupsByName(String name, String purpose) {
    var requestPurpose = new RequestPurpose(purpose);

    return organisationApi.searchOrganisationGroups(name, ORGANISATION_GROUPS_PROJECTION_ROOT, requestPurpose);
  }

  public RestSearchResult organisationGroupsToSearchResult(List<OrganisationGroup> queryResults) {
    return new RestSearchResult(queryResults.stream()
        .map(organisationGroup -> new RestSearchItem(
            organisationGroup.getOrganisationGroupId().toString(),
            organisationGroup.getName()))
        .toList());
  }

  public Optional<OrganisationGroup> getOrganisationGroupById(Integer id, String purpose) {
    var organisationGroupFilter = new OrganisationGroupProjectionRoot()
        .organisationGroupId()
        .name();
    var requestPurpose = new RequestPurpose(purpose);

    return organisationApi.findOrganisationGroup(id, organisationGroupFilter, requestPurpose);
  }

  public List<OrganisationGroup> getOrganisationGroupsByIds(List<Integer> ids, String purpose) {
    var requestPurpose = new RequestPurpose(purpose);
    return organisationApi.getAllOrganisationGroupsByIds(ids, ORGANISATION_GROUPS_PROJECTION_ROOT, requestPurpose);
  }

}
