package uk.co.nstauthority.scap.scap.actualtender.activity;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.assertj.core.api.Assertions.entry;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.List;
import java.util.Set;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.validation.BeanPropertyBindingResult;
import org.springframework.validation.BindingResult;
import uk.co.nstauthority.scap.fds.searchselector.ManualEntryUtil;
import uk.co.nstauthority.scap.scap.RemunerationModel;
import uk.co.nstauthority.scap.scap.actualtender.ActualTender;
import uk.co.nstauthority.scap.utils.ValidatorTestingUtil;

@ExtendWith(MockitoExtension.class)
class ActualTenderActivityFormValidatorTest {

  @Mock
  ActualTenderActivityService actualTenderActivityService;

  @InjectMocks
  ActualTenderActivityFormValidator validator;

  private ActualTenderActivityForm form;
  private BindingResult bindingResult;
  private ActualTender actualTender;

  @BeforeEach
  void setup() {
    form = new ActualTenderActivityForm();
    form.setInvitationToTenderParticipants(Collections.emptyList());
    bindingResult = new BeanPropertyBindingResult(form, "form");
    actualTender = new ActualTender(156);
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
  void validate_NoValidationHint_AssertThrows() {
    assertThatThrownBy(() -> validator.validate(form, bindingResult)).isInstanceOf(IllegalArgumentException.class);
  }

  @Test
  void validate_InvalidValidationHint_AssertThrows() {
    var object = new Object();
    assertThatThrownBy(() -> validator.validate(form, bindingResult, object)).isInstanceOf(IllegalStateException.class);
  }

  @Test
  void validate_simpleValidForm_assertNoErrors() {
    form.setScopeTitle("test scope title");
    form.setScopeDescription("test scope description");
    form.setRemunerationModel(RemunerationModel.OTHER);
    form.setRemunerationModelName("test remuneration model");
    form.setContractStage(ContractStage.CONTRACT_AWARDED);
    form.setInvitationToTenderParticipants(Collections.singletonList(ManualEntryUtil.addFreeTextPrefix("test participant")));

    validator.validate(form, bindingResult, new ActualTenderFormValidatorHint(actualTender));

    assertFalse(bindingResult.hasErrors());
  }

  @Test
  void validate_emptyForm_assertRequiredErrors() {
    validator.validate(form, bindingResult, new ActualTenderFormValidatorHint(actualTender));

    var extractedErrors = ValidatorTestingUtil.extractErrors(bindingResult);

    assertThat(extractedErrors).containsExactly(
        entry("scopeTitle.inputValue", Set.of("scopeTitle.required")),
        entry("scopeDescription.inputValue", Set.of("scopeDescription.required")),
        entry("remunerationModel", Set.of("remunerationModel.required")),
        entry("contractStage", Set.of("contractStage.required")),
        entry("ittParticipantsSelector", Set.of("ittParticipantsSelector.required"))
    );
  }

  @Test
  void validate_emptyFormWithNullIttParticipants_assertRequiredErrors() {
    form.setInvitationToTenderParticipants(null);

    validator.validate(form, bindingResult, new ActualTenderFormValidatorHint(actualTender));

    var extractedErrors = ValidatorTestingUtil.extractErrors(bindingResult);

    assertThat(extractedErrors).containsExactly(
        entry("scopeTitle.inputValue", Set.of("scopeTitle.required")),
        entry("scopeDescription.inputValue", Set.of("scopeDescription.required")),
        entry("remunerationModel", Set.of("remunerationModel.required")),
        entry("contractStage", Set.of("contractStage.required")),
        entry("ittParticipantsSelector", Set.of("ittParticipantsSelector.required"))
    );
  }

  @Test
  void validate_otherRemunerationModel_assertRemunerationModelNameRequired() {
    form.setScopeTitle("test scope title");
    form.setScopeDescription("test scope description");
    form.setRemunerationModel(RemunerationModel.OTHER);
    form.setContractStage(ContractStage.CONTRACT_AWARDED);
    form.setInvitationToTenderParticipants(Collections.singletonList(ManualEntryUtil.addFreeTextPrefix("test participant")));

    validator.validate(form, bindingResult, new ActualTenderFormValidatorHint(actualTender));
    var extractedErrors = ValidatorTestingUtil.extractErrors(bindingResult);

    assertThat(extractedErrors).containsExactly(
        entry("remunerationModelName.inputValue", Set.of("remunerationModelName.required"))
    );
  }

  @Test
  void validate_NonUniqueScope_AssertError() {
    form.setScopeTitle("test scope title");
    form.setScopeDescription("test scope description");
    form.setRemunerationModel(RemunerationModel.LUMP_SUM);
    form.setContractStage(ContractStage.CONTRACT_AWARDED);
    form.setInvitationToTenderParticipants(Collections.singletonList(ManualEntryUtil.addFreeTextPrefix("test participant")));
    var existingActualTenderActivity = new ActualTenderActivity(156);
    existingActualTenderActivity.setScopeTitle("Test SCOPE title");

    when(actualTenderActivityService.getAllByActualTender(actualTender))
        .thenReturn(List.of(existingActualTenderActivity));

    validator.validate(form, bindingResult, new ActualTenderFormValidatorHint(actualTender));
    var extractedErrors = ValidatorTestingUtil.extractErrors(bindingResult);

    assertThat(extractedErrors).containsExactly(
        entry("scopeTitle.inputValue", Set.of("scopeTitle.notUnique"))
    );
  }

  @Test
  void validate_ChangingOwnScopeTitle_AssertNoErrors() {
    form.setScopeTitle("test scope title");
    form.setScopeDescription("test scope description");
    form.setRemunerationModel(RemunerationModel.LUMP_SUM);
    form.setContractStage(ContractStage.CONTRACT_AWARDED);
    form.setInvitationToTenderParticipants(Collections.singletonList(ManualEntryUtil.addFreeTextPrefix("test participant")));
    var existingActualTenderActivity = new ActualTenderActivity(156);
    existingActualTenderActivity.setScopeTitle("Test SCOPE title");

    when(actualTenderActivityService.getAllByActualTender(actualTender))
        .thenReturn(List.of(existingActualTenderActivity));

    validator.validate(form, bindingResult, new ActualTenderFormValidatorHint(actualTender),
        new ActualTenderActivityFormValidatorHint(existingActualTenderActivity.getId()));

    assertFalse(bindingResult.hasErrors());
  }

  @Test
  void validate_ChangingOtherExistingScopeTitle_AssertErrors() {
    form.setScopeTitle("test scope title");
    form.setScopeDescription("test scope description");
    form.setRemunerationModel(RemunerationModel.LUMP_SUM);
    form.setContractStage(ContractStage.CONTRACT_AWARDED);
    form.setInvitationToTenderParticipants(Collections.singletonList(ManualEntryUtil.addFreeTextPrefix("test participant")));
    var existingActualTenderActivity = new ActualTenderActivity(156);
    existingActualTenderActivity.setScopeTitle("Test SCOPE title");

    when(actualTenderActivityService.getAllByActualTender(actualTender))
        .thenReturn(List.of(existingActualTenderActivity));

    validator.validate(form, bindingResult, new ActualTenderFormValidatorHint(actualTender),
        new ActualTenderActivityFormValidatorHint(999));
    var extractedErrors = ValidatorTestingUtil.extractErrors(bindingResult);

    assertThat(extractedErrors).containsExactly(
        entry("scopeTitle.inputValue", Set.of("scopeTitle.notUnique"))
    );
  }

  @Test
  void validate_ChangingOtherExistingScopeTitleWithWhitespace_AssertErrors() {
    form.setScopeTitle("      test scope title    ");
    form.setScopeDescription("test scope description");
    form.setRemunerationModel(RemunerationModel.LUMP_SUM);
    form.setContractStage(ContractStage.CONTRACT_AWARDED);
    form.setInvitationToTenderParticipants(Collections.singletonList(ManualEntryUtil.addFreeTextPrefix("test participant")));
    var existingActualTenderActivity = new ActualTenderActivity(156);
    existingActualTenderActivity.setScopeTitle("Test SCOPE title");

    when(actualTenderActivityService.getAllByActualTender(actualTender))
        .thenReturn(List.of(existingActualTenderActivity));

    validator.validate(form, bindingResult, new ActualTenderFormValidatorHint(actualTender),
        new ActualTenderActivityFormValidatorHint(999));
    var extractedErrors = ValidatorTestingUtil.extractErrors(bindingResult);

    assertThat(extractedErrors).containsExactly(
        entry("scopeTitle.inputValue", Set.of("scopeTitle.notUnique"))
    );
  }

  @Test
  void validate_ScopeTitleTooLong_AssertError() {
    var tooLongScopeTitle = StringUtils.repeat("X", ActualTenderActivityFormValidator.MAX_SCOPE_TITLE_LENGTH + 1);
    form.setScopeTitle(tooLongScopeTitle);
    form.setScopeDescription("test scope description");
    form.setRemunerationModel(RemunerationModel.LUMP_SUM);
    form.setContractStage(ContractStage.CONTRACT_AWARDED);
    form.setInvitationToTenderParticipants(Collections.singletonList(ManualEntryUtil.addFreeTextPrefix("test participant")));

    validator.validate(form, bindingResult, new ActualTenderFormValidatorHint(actualTender));
    var extractedErrors = ValidatorTestingUtil.extractErrors(bindingResult);

    assertThat(extractedErrors).containsExactly(
        entry("scopeTitle.inputValue", Set.of("scopeTitle.maxCharExceeded"))
    );
  }

  @Test
  void validate_InvalidIttParticipants_AssertErrors() {
    form.setScopeTitle("test scope title");
    form.setScopeDescription("test scope description");
    form.setRemunerationModel(RemunerationModel.OTHER);
    form.setRemunerationModelName("test remuneration model");
    form.setContractStage(ContractStage.CONTRACT_AWARDED);
    form.setInvitationToTenderParticipants(Collections.singletonList("NaN"));

    validator.validate(form, bindingResult, new ActualTenderFormValidatorHint(actualTender));
    var extractedErrors = ValidatorTestingUtil.extractErrors(bindingResult);
    var fieldName = ActualTenderActivityFormValidator.ITT_PARTICIPANTS_SELECTOR_NAME;

    assertThat(extractedErrors).containsExactly(
        entry(fieldName, Collections.singleton("%s.invalid".formatted(fieldName)))
    );
  }
}
