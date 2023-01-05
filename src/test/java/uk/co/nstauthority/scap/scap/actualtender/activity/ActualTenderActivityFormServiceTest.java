package uk.co.nstauthority.scap.scap.actualtender.activity;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;

import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.validation.BeanPropertyBindingResult;
import uk.co.nstauthority.scap.scap.RemunerationModel;
import uk.co.nstauthority.scap.scap.actualtender.ActualTender;

@ExtendWith(MockitoExtension.class)
class ActualTenderActivityFormServiceTest {

  @Mock
  ActualTenderActivityFormValidator actualTenderActivityFormValidator;

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
    var invitationToTenderParticipants = List.of(invitationToTenderParticipant1);

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
        List.of(invitationToTenderParticipant1.getCompanyName())
    );
  }
}
