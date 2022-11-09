package uk.co.nstauthority.scap.scap.actualtender.activity;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.entry;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Set;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.validation.BeanPropertyBindingResult;
import org.springframework.validation.BindingResult;
import uk.co.nstauthority.scap.scap.RemunerationModel;
import uk.co.nstauthority.scap.utils.ValidatorTestingUtil;

@ExtendWith(MockitoExtension.class)
class ActualTenderActivityFormValidatorTest {

  private ActualTenderActivityFormValidator validator;
  private ActualTenderActivityForm form;
  private BindingResult bindingResult;

  @BeforeEach
  void setup() {
    validator = new ActualTenderActivityFormValidator();
    form = new ActualTenderActivityForm();
    bindingResult = new BeanPropertyBindingResult(form, "form");
  }

  @Test
  void supports_actualTenderDetailForm_assertTrue() {
    assertTrue(validator.supports(ActualTenderActivityForm.class));
  }

  @Test
  void supports_nonSupportedClass_assertFalse() {
    assertFalse(validator.supports(ValidatorTestingUtil.NonSupportedClass.class));
  }

  @Test
  void validate_simpleValidForm_assertNoErrors() {
    form.setScopeTitle("test scope title");
    form.setScopeDescription("test scope description");
    form.setRemunerationModel(RemunerationModel.OTHER);
    form.setRemunerationModelName("test remuneration model");
    form.setContractStage(ContractStage.CONTRACT_AWARDED);
    form.setInvitationToTenderParticipants("test participant");

    validator.validate(form, bindingResult);

    assertFalse(bindingResult.hasErrors());
  }

  @Test
  void validate_emptyForm_assertRequiredErrors() {
    validator.validate(form, bindingResult);

    var extractedErrors = ValidatorTestingUtil.extractErrors(bindingResult);

    assertThat(extractedErrors).containsExactly(
        entry("scopeTitle.inputValue", Set.of("scopeTitle.required")),
        entry("scopeDescription.inputValue", Set.of("scopeDescription.required")),
        entry("remunerationModel", Set.of("remunerationModel.required")),
        entry("contractStage", Set.of("contractStage.required")),
        entry("invitationToTenderParticipants.inputValue", Set.of("invitationToTenderParticipants.required"))
    );
  }

  @Test
  void validate_otherRemunerationModel_assertRemunerationModelNameRequired() {
    form.setScopeTitle("test scope title");
    form.setScopeDescription("test scope description");
    form.setRemunerationModel(RemunerationModel.OTHER);
    form.setContractStage(ContractStage.CONTRACT_AWARDED);
    form.setInvitationToTenderParticipants("test participant");

    validator.validate(form, bindingResult);
    var extractedErrors = ValidatorTestingUtil.extractErrors(bindingResult);

    assertThat(extractedErrors).containsExactly(
        entry("remunerationModelName.inputValue", Set.of("remunerationModelName.required"))
    );
  }
}
