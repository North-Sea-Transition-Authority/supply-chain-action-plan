package uk.co.nstauthority.scap.scap.actualtender.activity;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.verify;

import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.validation.BeanPropertyBindingResult;
import uk.co.nstauthority.scap.scap.RemunerationModel;

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

    var returnedBindingResult = actualTenderActivityFormService.validate(form, bindingResult);

    verify(actualTenderActivityFormValidator).validate(form, bindingResult);
    assertThat(returnedBindingResult).isEqualTo(bindingResult);
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
        form1 -> form1.getInvitationToTenderParticipants().getInputValue()
    ).containsExactly(
        actualTenderActivity.getScopeTitle(),
        actualTenderActivity.getScopeDescription(),
        actualTenderActivity.getRemunerationModel(),
        actualTenderActivity.getRemunerationModelName(),
        actualTenderActivity.getContractStage(),
        invitationToTenderParticipant1.getCompanyName()
    );
  }
}
