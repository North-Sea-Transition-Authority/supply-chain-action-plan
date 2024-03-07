package uk.co.nstauthority.scap.workarea;

import java.util.Objects;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.energyportal.FieldService;
import uk.co.nstauthority.scap.fds.searchselector.RestSearchItem;
import uk.co.nstauthority.scap.scap.organisationgroup.OrganisationGroupService;

@Service
class WorkAreaFormService {

  static final String ORGANISATION_SEARCH_REQUEST_PURPOSE = "Get preselected organisation for SCAP work area";
  static final String FIELD_SEARCH_REQUEST_PURPOSE = "Get preselected field for SCAP work area";
  static final RestSearchItem EMPTY_PREFILLED_ITEM = new RestSearchItem("", "");

  private final OrganisationGroupService organisationGroupService;
  private final FieldService fieldService;

  @Autowired
  WorkAreaFormService(OrganisationGroupService organisationGroupService, FieldService fieldService) {
    this.organisationGroupService = organisationGroupService;
    this.fieldService = fieldService;
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

  RestSearchItem getPreselectedField(Integer fieldId) {
    if (Objects.isNull(fieldId)) {
      return EMPTY_PREFILLED_ITEM;
    }

    var fieldOpt = fieldService
        .getFieldById(fieldId, FIELD_SEARCH_REQUEST_PURPOSE);

    return fieldOpt
        .map(field -> new RestSearchItem(fieldId.toString(), field.getFieldName()))
        .orElse(EMPTY_PREFILLED_ITEM);
  }
}
