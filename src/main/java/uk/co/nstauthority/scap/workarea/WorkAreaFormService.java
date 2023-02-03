package uk.co.nstauthority.scap.workarea;

import java.util.Objects;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.fds.searchselector.RestSearchItem;
import uk.co.nstauthority.scap.scap.organisationgroup.OrganisationGroupService;

@Service
class WorkAreaFormService {

  static final String ORGANISATION_SEARCH_REQUEST_PURPOSE = "Get preselect organisation for SCAP work area";
  static final RestSearchItem EMPTY_PREFILLED_ITEM = new RestSearchItem("", "");

  private final OrganisationGroupService organisationGroupService;

  @Autowired
  WorkAreaFormService(OrganisationGroupService organisationGroupService) {
    this.organisationGroupService = organisationGroupService;
  }

  RestSearchItem getPreselectedOrganisation(Integer organisationGroupId) {
    if (Objects.isNull(organisationGroupId)) {
      return EMPTY_PREFILLED_ITEM;
    }

    var organisationGroupOpt = organisationGroupService
        .getOrganisationGroupById(organisationGroupId, ORGANISATION_SEARCH_REQUEST_PURPOSE);

    return organisationGroupOpt
        .map(organisationGroup -> new RestSearchItem(organisationGroupId.toString(), organisationGroup.getName()))
        .orElse(EMPTY_PREFILLED_ITEM);
  }
}
