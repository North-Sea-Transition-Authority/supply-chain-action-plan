package uk.co.nstauthority.scap.scap.actualtender.activity;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.BindingResult;
import uk.co.nstauthority.scap.energyportal.OrganisationUnitService;
import uk.co.nstauthority.scap.fds.addtolist.AddToListItem;
import uk.co.nstauthority.scap.fds.searchselector.ManualEntryUtil;
import uk.co.nstauthority.scap.scap.actualtender.ActualTender;

@Service
class ActualTenderActivityFormService {

  private final ActualTenderActivityFormValidator validator;
  private final OrganisationUnitService organisationUnitService;

  static final String ORG_UNIT_REQUEST_PURPOSE = "Get Org Units to pre-fill SCAP actual tender activity form";

  @Autowired
  ActualTenderActivityFormService(ActualTenderActivityFormValidator validator,
                                  OrganisationUnitService organisationUnitService) {
    this.validator = validator;
    this.organisationUnitService = organisationUnitService;
  }

  BindingResult validate(ActualTenderActivityForm form, BindingResult bindingResult, ActualTender actualTender) {
    validator.validate(form, bindingResult, new ActualTenderFormValidatorHint(actualTender));
    return bindingResult;
  }

  BindingResult validate(ActualTenderActivityForm form,
                         BindingResult bindingResult,
                         ActualTender actualTender,
                         ActualTenderActivity actualTenderActivity) {
    validator.validate(
        form,
        bindingResult,
        new ActualTenderFormValidatorHint(actualTender),
        new ActualTenderActivityFormValidatorHint(actualTenderActivity.getId()));
    return bindingResult;
  }

  ActualTenderActivityForm getForm(ActualTenderActivity actualTenderActivity,
                                   List<InvitationToTenderParticipant> invitationToTenderParticipants) {
    var form = new ActualTenderActivityForm();
    form.setScopeTitle(actualTenderActivity.getScopeTitle());
    form.setScopeDescription(actualTenderActivity.getScopeDescription());
    form.setRemunerationModel(actualTenderActivity.getRemunerationModel());
    form.setRemunerationModelName(actualTenderActivity.getRemunerationModelName());
    form.setContractStage(actualTenderActivity.getContractStage());
    var participantIds = invitationToTenderParticipants.stream()
        .map(ittParticipant -> {
          if (Objects.nonNull(ittParticipant.getOrganisationUnitId())) {
            return String.valueOf(ittParticipant.getOrganisationUnitId());
          }
          return "%s%s".formatted(ManualEntryUtil.FREE_TEXT_PREFIX, ittParticipant.getCompanyName());
        })
        .toList();
    form.setInvitationToTenderParticipants(participantIds);
    return form;
  }

  public List<AddToListItem> getPreselectedIttParticipants(
      List<InvitationToTenderParticipant> invitationToTenderParticipants) {
    return invitationToTenderParticipants.stream()
        .map(ittParticipant -> {
          if (Objects.nonNull(ittParticipant.getOrganisationUnitId())) {
            return new AddToListItem(
                String.valueOf(ittParticipant.getOrganisationUnitId()), ittParticipant.getCompanyName(), true);
          }
          return new AddToListItem(
              "%s%s".formatted(ManualEntryUtil.FREE_TEXT_PREFIX, ittParticipant.getCompanyName()),
              ittParticipant.getCompanyName(),
              true
          );
        }).toList();
  }

  public List<AddToListItem> getPreselectedIttParticipants(List<String> invitationToTenderParticipants,
                                                           BindingResult bindingResult) {
    if (bindingResult.hasFieldErrors(ActualTenderActivityFormValidator.ITT_PARTICIPANTS_SELECTOR_NAME)) {
      return Collections.emptyList();
    }
    var partitionedList = ManualEntryUtil.partitionManualEntries(invitationToTenderParticipants);
    var manualEntries = partitionedList.manualEntries()
        .stream()
        .map(manualEntry -> new AddToListItem(ManualEntryUtil.addFreeTextPrefix(manualEntry), manualEntry, true))
        .toList();
    var addToListItems = new ArrayList<>(manualEntries);
    var organisationUnits = organisationUnitService
        .findAllByIds(partitionedList.ids(), ORG_UNIT_REQUEST_PURPOSE);
    var epaEntries = organisationUnits.stream()
        .map(organisationUnit -> new AddToListItem(
            String.valueOf(organisationUnit.getOrganisationUnitId()), organisationUnit.getName(), true))
        .toList();
    addToListItems.addAll(epaEntries);
    return addToListItems;
  }
}
