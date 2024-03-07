package uk.co.nstauthority.scap.scap.organisationgroup;

import java.util.Objects;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.BindingResult;
import uk.co.nstauthority.scap.error.exception.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.fds.searchselector.RestSearchItem;
import uk.co.nstauthority.scap.restapi.scap.ScapRestService;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;

@Service
public class OrganisationGroupFormService {

  static final String ORG_GROUP_REQUEST_PURPOSE =
      "Get organisation group for parent SCAP to pre-fill SCAP operator form";
  private final OrganisationGroupFormValidator organisationGroupFormValidator;
  private final OrganisationGroupService organisationGroupService;

  @Autowired
  public OrganisationGroupFormService(OrganisationGroupFormValidator organisationGroupFormValidator,
                                      OrganisationGroupService organisationGroupService) {
    this.organisationGroupFormValidator = organisationGroupFormValidator;
    this.organisationGroupService = organisationGroupService;
  }

  public BindingResult validate(OrganisationGroupForm form, BindingResult bindingResult) {
    organisationGroupFormValidator.validate(form, bindingResult);
    return bindingResult;
  }

  public OrganisationGroupForm getForm(ScapDetail scapDetail) {
    var form = new OrganisationGroupForm();
    form.setOrganisationGroupId(scapDetail.getScap().getOrganisationGroupId());
    form.setIsTierOneContractor(scapDetail.isTierOneContractor());
    if (Objects.nonNull(scapDetail.getParentScap())) {
      form.setParentScapId(scapDetail.getParentScap().getId());
    }
    return form;
  }

  public RestSearchItem getPreselectedScap(ScapDetail scapDetail) {
    if (!Boolean.TRUE.equals(scapDetail.isTierOneContractor())) {
      return null;
    }
    var parentScap = scapDetail.getParentScap();
    if (Objects.isNull(parentScap)) {
      return null;
    }
    var orgGroup = organisationGroupService.getOrganisationGroupById(
        parentScap.getOrganisationGroupId(),
        ORG_GROUP_REQUEST_PURPOSE
    ).orElseThrow(() -> new ScapEntityNotFoundException("Could not find org group with ID [%d]"
        .formatted(parentScap.getOrganisationGroupId())));
    return ScapRestService.formatSearchResult(parentScap, orgGroup);
  }

  public RestSearchItem getPreselectedScap(ScapDetail scapDetail, BindingResult bindingResult) {
    if (bindingResult.hasFieldErrors(OrganisationGroupFormValidator.SCAP_ID_FIELD_NAME)) {
      return null;
    }
    return getPreselectedScap(scapDetail);
  }
}
