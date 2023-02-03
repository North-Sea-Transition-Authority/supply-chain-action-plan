package uk.co.nstauthority.scap.workarea;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.fivium.energyportalapi.generated.types.OrganisationGroup;
import uk.co.nstauthority.scap.fds.searchselector.RestSearchItem;
import uk.co.nstauthority.scap.scap.organisationgroup.OrganisationGroupService;

@ExtendWith(MockitoExtension.class)
class WorkAreaFormServiceTest {

  @Mock
  OrganisationGroupService organisationGroupService;

  @InjectMocks
  WorkAreaFormService workAreaFormService;

  @Test
  void getPreselectedOrganisation_NullOrganisationGroupId() {
    var preselectedOrganisation = workAreaFormService.getPreselectedOrganisation(null);

    verifyNoInteractions(organisationGroupService);
    assertThat(preselectedOrganisation).isEqualTo(WorkAreaFormService.EMPTY_PREFILLED_ITEM);
  }

  @Test
  void getPreselectedOrganisation_NonExistentOrganisation() {
    var organisationId = 1;

    var preselectedOrganisation = workAreaFormService.getPreselectedOrganisation(organisationId);

    verify(organisationGroupService).getOrganisationGroupById(organisationId, WorkAreaFormService.ORGANISATION_SEARCH_REQUEST_PURPOSE);
    verifyNoMoreInteractions(organisationGroupService);

    assertThat(preselectedOrganisation).isEqualTo(WorkAreaFormService.EMPTY_PREFILLED_ITEM);
  }

  @Test
  void getPreselectedOrganisation() {
    var organisationId = 1;
    var organisationName = "org name";
    var organisationGroup = OrganisationGroup.newBuilder()
        .organisationGroupId(organisationId)
        .name(organisationName)
        .build();

    when(organisationGroupService.getOrganisationGroupById(organisationId, WorkAreaFormService.ORGANISATION_SEARCH_REQUEST_PURPOSE))
        .thenReturn(Optional.ofNullable(organisationGroup));

    var preselectedOrganisation = workAreaFormService.getPreselectedOrganisation(organisationId);

    verify(organisationGroupService).getOrganisationGroupById(organisationId, WorkAreaFormService.ORGANISATION_SEARCH_REQUEST_PURPOSE);
    verifyNoMoreInteractions(organisationGroupService);

    assertThat(preselectedOrganisation).extracting(
        RestSearchItem::id,
        RestSearchItem::text
    ).containsExactly(
        String.valueOf(organisationId),
        organisationName
    );
  }
}
