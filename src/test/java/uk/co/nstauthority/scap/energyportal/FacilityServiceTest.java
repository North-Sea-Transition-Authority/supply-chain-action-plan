package uk.co.nstauthority.scap.energyportal;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.entry;
import static org.assertj.core.api.Assertions.tuple;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;

import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.fivium.energyportalapi.client.LogCorrelationId;
import uk.co.fivium.energyportalapi.client.RequestPurpose;
import uk.co.fivium.energyportalapi.client.facility.FacilityApi;
import uk.co.fivium.energyportalapi.generated.client.FacilitiesByIdsProjectionRoot;
import uk.co.fivium.energyportalapi.generated.client.FacilitiesByNameProjectionRoot;
import uk.co.fivium.energyportalapi.generated.types.Facility;
import uk.co.fivium.energyportalapi.generated.types.FacilityStatus;
import uk.co.fivium.energyportalapi.generated.types.FacilityType;
import uk.co.nstauthority.scap.fds.searchselector.RestSearchItem;

@ExtendWith(MockitoExtension.class)
class FacilityServiceTest {

  @Mock
  FacilityApi facilityApi;

  @InjectMocks
  FacilityService facilityService;

  @Test
  void searchFacilities() {
    var searchTerm = "test search term";
    var purpose = "test purpose";
    var facilities = List.of(
        new Facility(39, "Test facility", FacilityType.FLARE_TOWER, FacilityStatus.OPERATIONAL, true)
    );

    var requestPurposeArgumentCaptor = ArgumentCaptor.forClass(RequestPurpose.class);
    var facilitiesByNameProjectionRootArgumentCaptor = ArgumentCaptor.forClass(FacilitiesByNameProjectionRoot.class);

    when(facilityApi.searchFacilitiesByName(
        eq(searchTerm),
        facilitiesByNameProjectionRootArgumentCaptor.capture(),
        requestPurposeArgumentCaptor.capture(),
        any(LogCorrelationId.class)))
        .thenReturn(facilities);

    var returnedFacilities = facilityService.searchFacilities(searchTerm, purpose);

    assertThat(returnedFacilities).isEqualTo(facilities);
    assertThat(requestPurposeArgumentCaptor.getValue().purpose()).isEqualTo(purpose);
    assertThat(facilitiesByNameProjectionRootArgumentCaptor.getValue().getFields()).containsExactly(
        entry("id", null),
        entry("name", null)
    );
  }

  @Test
  void findFacilitiesByIds() {
    var ids = List.of(39);
    var purpose = "test purpose";
    var facilities = List.of(
        new Facility(39, "Test facility", FacilityType.FLARE_TOWER, FacilityStatus.OPERATIONAL, true)
    );

    var requestPurposeArgumentCaptor = ArgumentCaptor.forClass(RequestPurpose.class);
    var facilitiesByIdsProjectionRootArgumentCaptor = ArgumentCaptor.forClass(FacilitiesByIdsProjectionRoot.class);

    when(facilityApi.searchFacilitiesByIds(
        eq(ids),
        facilitiesByIdsProjectionRootArgumentCaptor.capture(),
        requestPurposeArgumentCaptor.capture(),
        any(LogCorrelationId.class)))
        .thenReturn(facilities);

    var returnedFacilities = facilityService.findFacilitiesByIds(ids, purpose);

    assertThat(returnedFacilities).isEqualTo(facilities);
    assertThat(requestPurposeArgumentCaptor.getValue().purpose()).isEqualTo(purpose);
    assertThat(facilitiesByIdsProjectionRootArgumentCaptor.getValue().getFields()).containsExactly(
        entry("id", null),
        entry("name", null)
    );
  }

  @Test
  void facilitiesToRestSearchResult() {
    var facility = new Facility(39, "Test facility", FacilityType.FLARE_TOWER, FacilityStatus.OPERATIONAL, true);

    var restSearchResult = facilityService.facilitiesToRestSearchResult(List.of(facility));

    assertThat(restSearchResult.getResults()).extracting(
        RestSearchItem::id,
        RestSearchItem::text
    ).containsExactly(
        tuple(String.valueOf(facility.getId()), facility.getName())
    );
  }
}
