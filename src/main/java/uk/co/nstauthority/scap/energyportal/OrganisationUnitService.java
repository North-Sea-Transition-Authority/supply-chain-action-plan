package uk.co.nstauthority.scap.energyportal;

import java.util.Collections;
import java.util.List;
import java.util.Optional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.fivium.energyportalapi.client.RequestPurpose;
import uk.co.fivium.energyportalapi.client.organisation.OrganisationApi;
import uk.co.fivium.energyportalapi.generated.client.OrganisationUnitProjectionRoot;
import uk.co.fivium.energyportalapi.generated.client.OrganisationUnitsProjectionRoot;
import uk.co.fivium.energyportalapi.generated.types.OrganisationUnit;
import uk.co.nstauthority.scap.fds.searchselector.RestSearchItem;
import uk.co.nstauthority.scap.fds.searchselector.RestSearchResult;

@Service
public class OrganisationUnitService {

  private final OrganisationApi organisationApi;

  static final OrganisationUnitsProjectionRoot ORGANISATION_UNITS_PROJECTION_ROOT =
      new OrganisationUnitsProjectionRoot().organisationUnitId().name();
  static final OrganisationUnitProjectionRoot ORGANISATION_UNIT_PROJECTION_ROOT =
      new OrganisationUnitProjectionRoot().organisationUnitId().name();

  @Autowired
  OrganisationUnitService(OrganisationApi organisationApi) {
    this.organisationApi = organisationApi;
  }

  public List<OrganisationUnit> searchByName(String term, String purpose) {
    var requestPurpose = new RequestPurpose(purpose);

    return organisationApi.searchOrganisationUnits(term, ORGANISATION_UNITS_PROJECTION_ROOT, requestPurpose);
  }

  public RestSearchResult organisationUnitListToRestSearchResult(List<OrganisationUnit> organisationUnits) {
    var restSearchItems = organisationUnits.stream()
        .map(organisationUnit -> new RestSearchItem(
            String.valueOf(organisationUnit.getOrganisationUnitId()),
            organisationUnit.getName())
        ).toList();
    return new RestSearchResult(restSearchItems);
  }

  public List<OrganisationUnit> findAllByIds(List<Integer> organisationUnitIds, String purpose) {
    if (organisationUnitIds.isEmpty()) {
      return Collections.emptyList();
    }

    var requestPurpose = new RequestPurpose(purpose);

    return organisationUnitIds.stream()
        .map(id -> organisationApi.findOrganisationUnit(id, ORGANISATION_UNIT_PROJECTION_ROOT, requestPurpose))
        .flatMap(Optional::stream)
        .toList();
  }
}
