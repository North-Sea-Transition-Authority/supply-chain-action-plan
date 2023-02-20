package uk.co.nstauthority.scap.scap.actualtender.activity;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.tuple;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;
import static uk.co.nstauthority.scap.mvc.ReverseRouter.emptyBindingResult;

import java.util.Collections;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.validation.BeanPropertyBindingResult;
import uk.co.fivium.energyportalapi.generated.types.OrganisationUnit;
import uk.co.nstauthority.scap.energyportal.OrganisationUnitService;
import uk.co.nstauthority.scap.fds.addtolist.AddToListItem;
import uk.co.nstauthority.scap.fds.searchselector.ManualEntryUtil;
import uk.co.nstauthority.scap.scap.RemunerationModel;
import uk.co.nstauthority.scap.scap.actualtender.ActualTender;

@ExtendWith(MockitoExtension.class)
class ActualTenderActivityFormServiceTest {

  @Mock
  ActualTenderActivityFormValidator actualTenderActivityFormValidator;

  @Mock
  OrganisationUnitService organisationUnitService;

  @InjectMocks
  ActualTenderActivityFormService actualTenderActivityFormService;

  @Test
  void validate_verifyCallsValidator() {
    var form = new ActualTenderActivityForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");
    var actualTender = new ActualTender(156);
    var validatorHintCaptor = ArgumentCaptor.forClass(ActualTenderFormValidatorHint.class);

    var returnedBindingResult = actualTenderActivityFormService.validate(form, bindingResult, actualTender);

    verify(actualTenderActivityFormValidator).validate(eq(form), eq(bindingResult), validatorHintCaptor.capture());
    assertThat(returnedBindingResult).isEqualTo(bindingResult);
    assertThat(validatorHintCaptor.getValue().actualTender()).isEqualTo(actualTender);
  }

  @Test
  void validate_WithExistingId_VerifyCallsValidator() {
    var form = new ActualTenderActivityForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");
    var actualTender = new ActualTender(156);
    var actualTenderActivity = new ActualTenderActivity(176);
    var validatorHintCaptor = ArgumentCaptor.forClass(ActualTenderFormValidatorHint.class);
    var activityValidatorHintCaptor = ArgumentCaptor.forClass(ActualTenderActivityFormValidatorHint.class);

    var returnedBindingResult = actualTenderActivityFormService.validate(form, bindingResult, actualTender, actualTenderActivity);

    verify(actualTenderActivityFormValidator).validate(eq(form), eq(bindingResult), validatorHintCaptor.capture(),
        activityValidatorHintCaptor.capture());
    assertThat(returnedBindingResult).isEqualTo(bindingResult);
    assertThat(validatorHintCaptor.getValue().actualTender()).isEqualTo(actualTender);
    assertThat(activityValidatorHintCaptor.getValue().currentActivityId()).isEqualTo(actualTenderActivity.getId());
  }

  @Test
  void getForm() {
    var actualTenderActivity = new ActualTenderActivity(45);
    actualTenderActivity.setScopeTitle("test scope title");
    actualTenderActivity.setScopeDescription("test scope description");
    actualTenderActivity.setRemunerationModel(RemunerationModel.OTHER);
    actualTenderActivity.setRemunerationModelName("Other remuneration model");
    actualTenderActivity.setContractStage(ContractStage.CONTRACT_AWARDED);
    var invitationToTenderParticipant1 = new InvitationToTenderParticipant(451);
    invitationToTenderParticipant1.setCompanyName("test company 1");
    var invitationToTenderParticipant2 = new InvitationToTenderParticipant(452);
    invitationToTenderParticipant2.setCompanyName("portal org unit name");
    invitationToTenderParticipant2.setOrganisationUnitId(11);
    var invitationToTenderParticipants = List.of(
        invitationToTenderParticipant1, invitationToTenderParticipant2
    );

    var form = actualTenderActivityFormService.getForm(
        actualTenderActivity, invitationToTenderParticipants
    );

    assertThat(form).extracting(
        form1 -> form1.getScopeTitle().getInputValue(),
        form1 -> form1.getScopeDescription().getInputValue(),
        ActualTenderActivityForm::getRemunerationModel,
        form1 -> form1.getRemunerationModelName().getInputValue(),
        ActualTenderActivityForm::getContractStage,
        ActualTenderActivityForm::getInvitationToTenderParticipants
    ).containsExactly(
        actualTenderActivity.getScopeTitle(),
        actualTenderActivity.getScopeDescription(),
        actualTenderActivity.getRemunerationModel(),
        actualTenderActivity.getRemunerationModelName(),
        actualTenderActivity.getContractStage(),
        List.of(
            ManualEntryUtil.addFreeTextPrefix(invitationToTenderParticipant1.getCompanyName()),
            String.valueOf(invitationToTenderParticipant2.getOrganisationUnitId())
        )
    );
  }

  @Test
  void getPreselectedIttParticipants_FromIttParticipants() {
    var nonPortalIttParticipant = new InvitationToTenderParticipantBuilder()
        .withCompanyName("Manually added company name")
        .build();
    var ittParticipantFromPortal = new InvitationToTenderParticipantBuilder()
        .withOrganisationUnitId(1)
        .withCompanyName("EPA organisation unit name")
        .build();

    var addToListItems = actualTenderActivityFormService.getPreselectedIttParticipants(
        List.of(nonPortalIttParticipant, ittParticipantFromPortal)
    );

    assertThat(addToListItems).extracting(
        AddToListItem::getId,
        AddToListItem::getName
    ).containsExactly(
        tuple(ManualEntryUtil.addFreeTextPrefix(nonPortalIttParticipant.getCompanyName()), nonPortalIttParticipant.getCompanyName()),
        tuple(String.valueOf(ittParticipantFromPortal.getOrganisationUnitId()), ittParticipantFromPortal.getCompanyName())
    );
  }

  @Test
  void getPreselectedIttParticipants_FromForm_NoErrors() {
    var organisationUnit = OrganisationUnit.newBuilder()
        .organisationUnitId(55)
        .name("EPA org unit")
        .build();
    var manuallyAddedCompanyName = "Manually added company";

    var form = new ActualTenderActivityForm();
    form.setInvitationToTenderParticipants(List.of(
        String.valueOf(organisationUnit.getOrganisationUnitId()),
        ManualEntryUtil.addFreeTextPrefix(manuallyAddedCompanyName)
    ));

    when(organisationUnitService.findAllByIds(
        Collections.singletonList(organisationUnit.getOrganisationUnitId()),
        ActualTenderActivityFormService.ORG_UNIT_REQUEST_PURPOSE))
        .thenReturn(Collections.singletonList(organisationUnit));

    var addToListItems = actualTenderActivityFormService.getPreselectedIttParticipants(
        form.getInvitationToTenderParticipants(), emptyBindingResult()
    );

    assertThat(addToListItems).extracting(
        AddToListItem::getId,
        AddToListItem::getName
    ).containsExactly(
        tuple(ManualEntryUtil.addFreeTextPrefix(manuallyAddedCompanyName), manuallyAddedCompanyName),
        tuple(String.valueOf(organisationUnit.getOrganisationUnitId()), organisationUnit.getName())
    );
  }

  @Test
  void getPreselectedIttParticipants_FromForm_HasErrors() {
    var manuallyAddedCompanyName = "Manually added company";

    var form = new ActualTenderActivityForm();
    form.setInvitationToTenderParticipants(List.of(
        "9999",
        ManualEntryUtil.addFreeTextPrefix(manuallyAddedCompanyName)
    ));
    var bindingResult = new BeanPropertyBindingResult(form, "form");
    bindingResult.rejectValue(ActualTenderActivityFormValidator.ITT_PARTICIPANTS_SELECTOR_NAME, "invalid");

    var addToListItems = actualTenderActivityFormService.getPreselectedIttParticipants(
        form.getInvitationToTenderParticipants(), bindingResult
    );

    verifyNoInteractions(organisationUnitService);

    assertThat(addToListItems).isEmpty();
  }
}
