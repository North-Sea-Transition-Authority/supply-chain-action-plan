package uk.co.nstauthority.scap.energyportal;

import java.util.List;
import java.util.UUID;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.fivium.energyportalapi.client.LogCorrelationId;
import uk.co.fivium.energyportalapi.client.RequestPurpose;
import uk.co.fivium.energyportalapi.client.facility.FacilityApi;
import uk.co.fivium.energyportalapi.generated.client.FacilitiesByIdsProjectionRoot;
import uk.co.fivium.energyportalapi.generated.client.FacilitiesByNameProjectionRoot;
import uk.co.fivium.energyportalapi.generated.types.Facility;
import uk.co.nstauthority.scap.fds.searchselector.RestSearchItem;
import uk.co.nstauthority.scap.fds.searchselector.RestSearchResult;

@Service
public class FacilityService {

  private final FacilityApi facilityApi;

  @Autowired
  public FacilityService(FacilityApi facilityApi) {
    this.facilityApi = facilityApi;
  }

  public List<Facility> searchFacilities(String searchTerm, String purpose) {
    var requestPurpose = new RequestPurpose(purpose);
    var requestedFields = new FacilitiesByNameProjectionRoot()
        .id().name();

    return facilityApi.searchFacilitiesByName(searchTerm, requestedFields, requestPurpose, getLogCorrelationId());
  }

  public List<Facility> findFacilitiesByIds(List<Integer> facilityIds, String purpose) {
    var requestPurpose = new RequestPurpose(purpose);
    var requestedFields = new FacilitiesByIdsProjectionRoot()
        .id().name();

    return facilityApi.searchFacilitiesByIds(
        facilityIds,
        requestedFields,
        requestPurpose,
        getLogCorrelationId()
    );
  }

  public RestSearchResult facilitiesToRestSearchResult(List<Facility> facilities) {
    return new RestSearchResult(
        facilities.stream()
            .map(facility -> new RestSearchItem(String.valueOf(facility.getId()), facility.getName()))
            .toList()
    );
  }

  private LogCorrelationId getLogCorrelationId() {
    return new LogCorrelationId(UUID.randomUUID().toString());
  }
}
