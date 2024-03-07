package uk.co.nstauthority.scap.energyportal;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.List;
import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.fivium.energyportalapi.client.RequestPurpose;
import uk.co.fivium.energyportalapi.client.organisation.OrganisationApi;
import uk.co.fivium.energyportalapi.generated.client.OrganisationUnitProjectionRoot;
import uk.co.fivium.energyportalapi.generated.client.OrganisationUnitsProjectionRoot;
import uk.co.fivium.energyportalapi.generated.types.OrganisationUnit;

@ExtendWith(MockitoExtension.class)
class OrganisationUnitServiceTest {

  @Captor
  ArgumentCaptor<RequestPurpose> requestPurposeArgumentCaptor;

  @Mock
  OrganisationApi organisationApi;

  @InjectMocks
  OrganisationUnitService organisationUnitService;

  @Test
  void searchByName() {
    var orgUnit = OrganisationUnit.newBuilder()
        .organisationUnitId(2)
        .name("test org")
        .build();
    var searchTerm = "test";
    var requestPurpose = "test req purpose";

    when(organisationApi.searchOrganisationUnits(
        anyString(), any(OrganisationUnitsProjectionRoot.class), any(RequestPurpose.class)))
        .thenReturn(Collections.singletonList(orgUnit));

    var returnedOrgUnits = organisationUnitService.searchByName(searchTerm, requestPurpose);

    verify(organisationApi).searchOrganisationUnits(
        eq(searchTerm),
        eq(OrganisationUnitService.ORGANISATION_UNITS_PROJECTION_ROOT),
        requestPurposeArgumentCaptor.capture());

    assertThat(returnedOrgUnits).containsExactly(orgUnit);
    assertThat(requestPurposeArgumentCaptor.getValue().purpose()).isEqualTo(requestPurpose);
  }

  @Test
  void findAllByIds_EmptyList_AssertNoApiCall() {
    var returnedOrgUnits = organisationUnitService.findAllByIds(Collections.emptyList(), "test");

    verifyNoInteractions(organisationApi);
    assertThat(returnedOrgUnits).isEmpty();
  }

  @Test
  void findAllByIds() {
    var orgUnitId1 = 1;
    var orgUnitId2 = 2;
    var requestPurpose = "test req purpose";

    var orgUnit1 = OrganisationUnit.newBuilder().organisationUnitId(orgUnitId1).name("test org 1").build();
    var orgUnit2 = OrganisationUnit.newBuilder().organisationUnitId(orgUnitId2).name("test org 2").build();

    var orgUnitIdArgumentCaptor = ArgumentCaptor.forClass(Integer.class);

    when(organisationApi.findOrganisationUnit(
        eq(orgUnitId1), any(OrganisationUnitProjectionRoot.class), any(RequestPurpose.class)))
        .thenReturn(Optional.of(orgUnit1));
    when(organisationApi.findOrganisationUnit(
        eq(orgUnitId2), any(OrganisationUnitProjectionRoot.class), any(RequestPurpose.class)))
        .thenReturn(Optional.of(orgUnit2));

    var returnedOrgUnits = organisationUnitService.findAllByIds(List.of(orgUnitId1, orgUnitId2), requestPurpose);

    verify(organisationApi, times(2)).findOrganisationUnit(
        orgUnitIdArgumentCaptor.capture(),
        eq(OrganisationUnitService.ORGANISATION_UNIT_PROJECTION_ROOT),
        requestPurposeArgumentCaptor.capture());

    assertThat(returnedOrgUnits).containsExactly(
        orgUnit1, orgUnit2
    );
    assertThat(requestPurposeArgumentCaptor.getAllValues())
        .extracting(RequestPurpose::purpose)
        .allMatch(requestPurpose::equals);
    assertThat(orgUnitIdArgumentCaptor.getAllValues()).containsExactly(
        orgUnitId1, orgUnitId2
    );
  }
}