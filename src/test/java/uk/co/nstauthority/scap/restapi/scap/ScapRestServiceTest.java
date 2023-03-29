package uk.co.nstauthority.scap.restapi.scap;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.tuple;
import static org.mockito.Mockito.when;

import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.fivium.energyportalapi.generated.types.OrganisationGroup;
import uk.co.nstauthority.scap.fds.searchselector.RestSearchItem;
import uk.co.nstauthority.scap.scap.organisationgroup.OrganisationGroupService;
import uk.co.nstauthority.scap.scap.scap.ScapEntityTestUtil;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.scap.ScapService;

@ExtendWith(MockitoExtension.class)
class ScapRestServiceTest {

  @Mock
  ScapService scapService;

  @Mock
  OrganisationGroupService organisationGroupService;

  @InjectMocks
  ScapRestService scapRestService;

  @Test
  void searchScaps() {
    var scapOperator = OrganisationGroup.newBuilder()
        .organisationGroupId(55)
        .name("CENTRICA")
        .build();
    var scapOperator2 = OrganisationGroup.newBuilder()
        .organisationGroupId(116)
        .name("SHELL")
        .build();
    var scap = ScapEntityTestUtil.scapBuilder()
        .withScapId(ScapId.valueOf(3))
        .withReference("SCAP/2022/3")
        .withOrganisationGroupId(scapOperator.getOrganisationGroupId())
        .build();
    var scap2 = ScapEntityTestUtil.scapBuilder()
        .withScapId(ScapId.valueOf(35))
        .withReference("SCAP/2022/35")
        .withOrganisationGroupId(scapOperator2.getOrganisationGroupId())
        .build();

    var searchTerm = "22/3";

    when(scapService.searchByReference(searchTerm)).thenReturn(List.of(scap, scap2));
    when(organisationGroupService.getOrganisationGroupsByIds(
        List.of(scap.getOrganisationGroupId(), scap2.getOrganisationGroupId()), ScapRestService.ORG_GROUP_REQUEST_PURPOSE
    )).thenReturn(List.of(scapOperator, scapOperator2));

    var result = scapRestService.searchScaps(searchTerm);

    assertThat(result.getResults()).extracting(
        RestSearchItem::id,
        RestSearchItem::text
    ).containsExactly(
        tuple(String.valueOf(scap.getId()), ScapRestService.formatSearchResult(scap, scapOperator).text()),
        tuple(String.valueOf(scap2.getId()), ScapRestService.formatSearchResult(scap2, scapOperator2).text())
    );
  }
}