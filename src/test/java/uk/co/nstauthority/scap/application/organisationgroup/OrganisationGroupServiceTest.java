package uk.co.nstauthority.scap.application.organisationgroup;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.entry;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
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
import uk.co.fivium.energyportalapi.client.organisation.OrganisationApi;
import uk.co.fivium.energyportalapi.generated.client.OrganisationGroupByIdProjectionRoot;
import uk.co.fivium.energyportalapi.generated.client.OrganisationGroupsByNameProjectionRoot;
import uk.co.fivium.energyportalapi.generated.types.OrganisationGroup;
import uk.co.nstauthority.scap.fds.searchselector.RestSearchItem;

@ExtendWith(MockitoExtension.class)
public class OrganisationGroupServiceTest {

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
    when(organisationApi.searchOrganisationGroupsByName(
        eq("company"),
        any(),
        eq("Test (getting companies who include 'company')")))
        .thenReturn(groupList);

    var organisationGroups =
        organisationGroupService.getOrganisationGroupsByName("company", "Test (getting companies who include 'company')");

    var argumentCaptor = ArgumentCaptor.forClass(OrganisationGroupsByNameProjectionRoot.class);

    verify(organisationApi, times(1)).searchOrganisationGroupsByName(
        eq("company"),
        argumentCaptor.capture(),
        eq("Test (getting companies who include 'company')")
    );

    assertThat(organisationGroups).isEqualTo(groupList);
    assertThat(argumentCaptor.getValue().getFields()).containsKeys("organisationGroupId", "name");
  }

  @Test
  void organisationGroupsToSearchResults() {
    var queryResults = List.of(
        new OrganisationGroup(
            1,
            "Royal Dutch Shell",
            "Shell",
            "shell.com",
            "ACTIVE",
            Collections.emptyList()));

    var searchResults = organisationGroupService
        .organisationGroupsToSearchResult(queryResults);

    assertThat(searchResults.getResults().size()).isEqualTo(1);

    assertThat(searchResults.getResults().get(0))
        .extracting(
            RestSearchItem::id,
            RestSearchItem::text
        ).containsExactly(
            "1",
            "Royal Dutch Shell"
        );
  }

  @Test
  void getOrganisationGroupById_verifyCallsApiWithCorrectParameters() {
    var purpose = "TEST: get organisation group by ID";
    var argumentCaptor = ArgumentCaptor
        .forClass(OrganisationGroupByIdProjectionRoot.class);
    var organisationGroup = new OrganisationGroup(
        1,
        "Royal Dutch Shell",
        "Shell",
        "shell.com",
        "ACTIVE",
        Collections.emptyList());

    when(organisationApi.searchOrganisationGroupById(eq(1), any(), eq(purpose)))
        .thenReturn(Optional.of(organisationGroup));

    var returnedOrganisation = organisationGroupService.getOrganisationGroupById(1, purpose);

    verify(organisationApi, times(1)).searchOrganisationGroupById(
        eq(1),
        argumentCaptor.capture(),
        eq(purpose));

    assertThat(returnedOrganisation.get()).isEqualTo(organisationGroup);
    assertThat(argumentCaptor.getValue().getFields())
        .containsOnly(
            entry("organisationGroupId", null),
            entry("name", null));
  }
}
