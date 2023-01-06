package uk.co.nstauthority.scap.scap.organisationgroup;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.entry;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.List;
import java.util.Optional;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.fivium.energyportalapi.client.RequestPurpose;
import uk.co.fivium.energyportalapi.client.organisation.OrganisationApi;
import uk.co.fivium.energyportalapi.generated.client.OrganisationGroupProjectionRoot;
import uk.co.fivium.energyportalapi.generated.client.OrganisationGroupsProjectionRoot;
import uk.co.fivium.energyportalapi.generated.types.OrganisationGroup;
import uk.co.nstauthority.scap.fds.searchselector.RestSearchItem;

@ExtendWith(MockitoExtension.class)
class OrganisationGroupServiceTest {

  @Mock
  private OrganisationApi organisationApi;

  @InjectMocks
  private OrganisationGroupService organisationGroupService;

  private List<OrganisationGroup> groupList;

  @BeforeEach
  void setup() {
    groupList = List.of(
        new OrganisationGroup(1, "Company 1", null, null, null, Collections.emptyList()),
        new OrganisationGroup(2, "Company 2", null, null, null, Collections.emptyList())
    );
  }

  @Test
  void getOrganisationGroupsByName() {
    var searchTerm = "company";

    when(organisationApi.searchOrganisationGroups(
        eq(searchTerm),
        any(OrganisationGroupsProjectionRoot.class),
        any(RequestPurpose.class)
    )).thenReturn(groupList);

    var organisationGroups =
        organisationGroupService.getOrganisationGroupsByName(searchTerm, "Test (getting companies who include 'company')");

    var argumentCaptor = ArgumentCaptor.forClass(OrganisationGroupsProjectionRoot.class);

    verify(organisationApi).searchOrganisationGroups(
        eq(searchTerm),
        argumentCaptor.capture(),
        any(RequestPurpose.class)
    );

    assertThat(organisationGroups).isEqualTo(groupList);
    assertThat(argumentCaptor.getValue().getFields()).containsKeys("organisationGroupId", "name");
  }

  @Test
  void organisationGroupsToSearchResults() {
    var organisationGroup = new OrganisationGroup(
        1,
        "Royal Dutch Shell",
        "Shell",
        "shell.com",
        "ACTIVE",
        Collections.emptyList());
    var queryResults = List.of(organisationGroup);

    var searchResults = organisationGroupService
        .organisationGroupsToSearchResult(queryResults);

    assertThat(searchResults.getResults()).hasSize(1);

    assertThat(searchResults.getResults().get(0))
        .extracting(
            RestSearchItem::id,
            RestSearchItem::text
        ).containsExactly(
            String.valueOf(organisationGroup.getOrganisationGroupId()),
            organisationGroup.getName()
        );
  }

  @Test
  void getOrganisationGroupById_verifyCallsApiWithCorrectParameters() {
    var purpose = "TEST: get organisation group by ID";
    var argumentCaptor = ArgumentCaptor
        .forClass(OrganisationGroupProjectionRoot.class);
    var organisationGroup = new OrganisationGroup(
        1,
        "Royal Dutch Shell",
        "Shell",
        "shell.com",
        "ACTIVE",
        Collections.emptyList());

    when(organisationApi.findOrganisationGroup(
        eq(organisationGroup.getOrganisationGroupId()),
        any(OrganisationGroupProjectionRoot.class),
        any(RequestPurpose.class)))
        .thenReturn(Optional.of(organisationGroup));

    var returnedOrganisation = organisationGroupService.getOrganisationGroupById(1, purpose);

    verify(organisationApi).findOrganisationGroup(
        eq(organisationGroup.getOrganisationGroupId()),
        argumentCaptor.capture(),
        any(RequestPurpose.class));

    assertThat(returnedOrganisation).contains(organisationGroup);
    assertThat(argumentCaptor.getValue().getFields())
        .containsOnly(
            entry("organisationGroupId", null),
            entry("name", null));
  }

  @Test
  void getOrganisationGroupsByIds_VerifyApiCall() {
    var purpose = "TEST: get organisation groups by IDs";
    var argumentCaptor = ArgumentCaptor.forClass(RequestPurpose.class);
    var organisationGroup = new OrganisationGroup(
        55,
        "CENTRICA",
        null,
        null,
        null,
        null
    );

    when(organisationApi.getAllOrganisationGroupsByIds(any(), any(), any()))
        .thenReturn(List.of(organisationGroup));

    var organisationGroups = organisationGroupService.getOrganisationGroupsByIds(
        List.of(organisationGroup.getOrganisationGroupId()), purpose);

    verify(organisationApi).getAllOrganisationGroupsByIds(
        eq(List.of(organisationGroup.getOrganisationGroupId())),
        eq(OrganisationGroupService.ORGANISATION_GROUPS_PROJECTION_ROOT),
        argumentCaptor.capture()
    );

    assertThat(organisationGroups).containsExactly(organisationGroup);
    assertThat(argumentCaptor.getValue().purpose()).isEqualTo(purpose);
  }
}
