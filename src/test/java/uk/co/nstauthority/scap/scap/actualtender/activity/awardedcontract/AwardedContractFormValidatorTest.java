package uk.co.nstauthority.scap.scap.actualtender.activity.awardedcontract;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.assertj.core.api.Assertions.entry;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.time.LocalDate;
import java.util.List;
import java.util.Set;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.validation.BeanPropertyBindingResult;
import org.springframework.validation.BindingResult;
import uk.co.nstauthority.scap.energyportal.CountryService;
import uk.co.nstauthority.scap.scap.actualtender.activity.InvitationToTenderParticipant;
import uk.co.nstauthority.scap.utils.ValidatorTestingUtil;

@ExtendWith(MockitoExtension.class)
class AwardedContractFormValidatorTest {

  @Mock
  CountryService countryService;

  @InjectMocks
  AwardedContractFormValidator validator;

  private AwardedContractForm form;
  private BindingResult bindingResult;
  private InvitationToTenderParticipant bidParticipant1;
  private List<InvitationToTenderParticipant> bidParticipants;

  @BeforeEach
  void setup() {
    bidParticipant1 = new InvitationToTenderParticipant(1410);
    var bidParticipant2 = new InvitationToTenderParticipant(1411);
    bidParticipants = List.of(bidParticipant1, bidParticipant2);

    form = getValidForm();
    bindingResult = new BeanPropertyBindingResult(form, "form");
  }

  @Test
  void supports_ActualContractForm_AssertTrue() {
    assertTrue(validator.supports(AwardedContractForm.class));
  }

  @Test
  void supports_NonSupportedClass_AssertFalse() {
    assertFalse(validator.supports(ValidatorTestingUtil.NonSupportedClass.class));
  }

  @Test
  void validate_NoValidationHint_AssertThrows() {
    var form = new AwardedContractForm();
    assertThatThrownBy(() ->validator.validate(form, new BeanPropertyBindingResult(form, "form")))
        .isInstanceOf(IllegalStateException.class);
  }

  @Test
  void validate_ValidForm_AssertNoErrors() {
    var countryId = 2141;
    form.setPreferredBidderId(bidParticipant1.getId());
    form.setPreferredBidderCountryId(countryId);

    when(countryService.doesCountryExist(countryId)).thenReturn(true);

    validator.validate(form, bindingResult, new AwardedContractFormValidatorHint(bidParticipants));

    assertFalse(bindingResult.hasErrors());
  }

  @Test
  void validate_EmptyForm_AssertErrors() {
    var form = new AwardedContractForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");
    validator.validate(form, bindingResult, new AwardedContractFormValidatorHint(bidParticipants));

    var extractedErrors = ValidatorTestingUtil.extractErrors(bindingResult);
    var contractAwardDateField = AwardedContractForm.CONTRACT_AWARD_DATE_FIELD;
    var contractStartDateField = AwardedContractForm.CONTRACT_START_DATE_FIELD;
    var contractEndDateField = AwardedContractForm.CONTRACT_END_DATE_FIELD;

    verify(countryService, never()).doesCountryExist(any());
    assertThat(extractedErrors).containsExactly(
        entry("preferredBidderId", Set.of("preferredBidderId.required")),
        entry("awardValue.inputValue", Set.of("awardValue.required")),
        entry("awardRationale.inputValue", Set.of("awardRationale.required")),
        entry("preferredBidderCountryId", Set.of("preferredBidderCountryId.required")),
        entry("%s.dayInput.inputValue".formatted(contractAwardDateField),
            Set.of("%s.dayInput.required".formatted(contractAwardDateField))),
        entry("%s.monthInput.inputValue".formatted(contractAwardDateField),
            Set.of("%s.monthInput.required".formatted(contractAwardDateField))),
        entry("%s.yearInput.inputValue".formatted(contractAwardDateField),
            Set.of("%s.yearInput.required".formatted(contractAwardDateField))),
        entry("%s".formatted(AwardedContractFormValidator.PAYMENT_TERMS_RADIO_FIELD),
            Set.of("%s.required".formatted(AwardedContractFormValidator.PAYMENT_TERMS_RADIO_FIELD))),
        entry("%s.dayInput.inputValue".formatted(contractStartDateField),
            Set.of("%s.dayInput.required".formatted(contractStartDateField))),
        entry("%s.monthInput.inputValue".formatted(contractStartDateField),
            Set.of("%s.monthInput.required".formatted(contractStartDateField))),
        entry("%s.yearInput.inputValue".formatted(contractStartDateField),
            Set.of("%s.yearInput.required".formatted(contractStartDateField))),
        entry("%s.dayInput.inputValue".formatted(contractEndDateField),
            Set.of("%s.dayInput.required".formatted(contractEndDateField))),
        entry("%s.monthInput.inputValue".formatted(contractEndDateField),
            Set.of("%s.monthInput.required".formatted(contractEndDateField))),
        entry("%s.yearInput.inputValue".formatted(contractEndDateField),
            Set.of("%s.yearInput.required".formatted(contractEndDateField)))
    );
  }

  @Test
  void validate_NonExistentBidParticipantAndCountry_AssertErrors() {
    var countryId = 9998;
    form.setPreferredBidderId(9999);
    form.setAwardValue("1.41");
    form.setAwardRationale("test award rationale");
    form.setPreferredBidderCountryId(countryId);
    form.setContractAwardDate(LocalDate.of(2000, 1, 1));
    form.setPaymentTermsRadio(PaymentTermsRadio.DAYS_30);

    when(countryService.doesCountryExist(countryId)).thenReturn(false);

    validator.validate(form, bindingResult, new AwardedContractFormValidatorHint(bidParticipants));

    var extractedErrors = ValidatorTestingUtil.extractErrors(bindingResult);

    assertThat(extractedErrors).containsExactly(
        entry("preferredBidderId", Set.of("preferredBidderId.doesNotExist")),
        entry("preferredBidderCountryId", Set.of("preferredBidderCountryId.doesNotExist"))
    );
  }

  @Test
  void validate_AwardValueTooSmall_AssertError() {
    var countryId = 0;
    form.setPreferredBidderId(bidParticipant1.getId());
    form.setAwardValue("0");
    form.setAwardRationale("test award rationale");
    form.setPreferredBidderCountryId(countryId);
    form.setContractAwardDate(LocalDate.of(2000, 1, 1));
    form.setPaymentTermsRadio(PaymentTermsRadio.DAYS_30);

    when(countryService.doesCountryExist(countryId)).thenReturn(true);

    validator.validate(form, bindingResult, new AwardedContractFormValidatorHint(bidParticipants));

    var extractedErrors = ValidatorTestingUtil.extractErrors(bindingResult);

    assertThat(extractedErrors).containsExactly(
        entry("awardValue.inputValue", Set.of("awardValue.minValueNotMet"))
    );
  }

  @Test
  void validate_AwardValueTooManyDecimalPlaces_AssertError() {
    var countryId = 0;
    form.setPreferredBidderId(bidParticipant1.getId());
    form.setAwardValue("0.1234");
    form.setAwardRationale("test award rationale");
    form.setPreferredBidderCountryId(countryId);
    form.setContractAwardDate(LocalDate.of(2000, 1, 1));
    form.setPaymentTermsRadio(PaymentTermsRadio.DAYS_30);

    when(countryService.doesCountryExist(countryId)).thenReturn(true);

    validator.validate(form, bindingResult, new AwardedContractFormValidatorHint(bidParticipants));

    var extractedErrors = ValidatorTestingUtil.extractErrors(bindingResult);

    assertThat(extractedErrors).containsExactly(
        entry("awardValue.inputValue", Set.of("awardValue.maxDecimalPlacesExceeded"))
    );
  }

  @Test
  void validate_NoPaymentTermsSelected_AssertErrors() {
    var countryId = 0;
    form.setPreferredBidderId(bidParticipant1.getId());
    form.setPreferredBidderCountryId(countryId);

    form.setPaymentTermsRadio(null);

    when(countryService.doesCountryExist(countryId)).thenReturn(true);

    validator.validate(form, bindingResult, new AwardedContractFormValidatorHint(bidParticipants));

    var extractedErrors = ValidatorTestingUtil.extractErrors(bindingResult);

    assertThat(extractedErrors).containsExactly(
        entry(AwardedContractFormValidator.PAYMENT_TERMS_RADIO_FIELD,
            Set.of("%s.required".formatted(AwardedContractFormValidator.PAYMENT_TERMS_RADIO_FIELD)))
    );
  }

  @Test
  void validate_OtherPaymentTerm_NoValueProvided_AssertErrors() {
    var countryId = 0;
    form.setPreferredBidderId(bidParticipant1.getId());
    form.setPreferredBidderCountryId(countryId);

    form.setPaymentTermsRadio(PaymentTermsRadio.OTHER);
    form.setOtherPaymentTerm(null);

    when(countryService.doesCountryExist(countryId)).thenReturn(true);

    validator.validate(form, bindingResult, new AwardedContractFormValidatorHint(bidParticipants));

    var extractedErrors = ValidatorTestingUtil.extractErrors(bindingResult);

    assertThat(extractedErrors).containsExactly(
        entry("otherPaymentTerm.inputValue",
            Set.of("otherPaymentTerm.required"))
    );
  }

  @Test
  void validate_InvalidPaymentTerm_AssertErrors() {
    var countryId = 0;
    form.setPreferredBidderId(bidParticipant1.getId());
    form.setPreferredBidderCountryId(countryId);

    form.setPaymentTermsRadio(PaymentTermsRadio.OTHER);
    form.setOtherPaymentTerm("-1");

    when(countryService.doesCountryExist(countryId)).thenReturn(true);

    validator.validate(form, bindingResult, new AwardedContractFormValidatorHint(bidParticipants));

    var extractedErrors = ValidatorTestingUtil.extractErrors(bindingResult);

    assertThat(extractedErrors).containsExactly(
        entry("otherPaymentTerm.inputValue",
            Set.of("otherPaymentTerm.minValueNotMet"))
    );
  }

  @Test
  void validate_ContractStartDateBeforeContractAwardDate_AssertError() {
    var bidParticipant1 = new InvitationToTenderParticipant(1410);
    var bidParticipant2 = new InvitationToTenderParticipant(1411);
    var bidParticipants = List.of(bidParticipant1, bidParticipant2);
    var countryId = 0;
    var contractStartDateField = AwardedContractForm.CONTRACT_START_DATE_FIELD;
    form.setPreferredBidderId(bidParticipant1.getId());
    form.setPreferredBidderCountryId(countryId);

    form.setContractStartDate(LocalDate.of(1990, 1, 1));

    when(countryService.doesCountryExist(countryId)).thenReturn(true);

    validator.validate(form, bindingResult, new AwardedContractFormValidatorHint(bidParticipants));

    var extractedErrors = ValidatorTestingUtil.extractErrors(bindingResult);

    assertThat(extractedErrors).containsExactly(
        entry("%s.dayInput.inputValue".formatted(contractStartDateField),
            Set.of("%s.dayInput.minDateExclusiveNotMet".formatted(contractStartDateField))),
        entry("%s.monthInput.inputValue".formatted(contractStartDateField),
            Set.of("%s.monthInput.minDateExclusiveNotMet".formatted(contractStartDateField))),
        entry("%s.yearInput.inputValue".formatted(contractStartDateField),
            Set.of("%s.yearInput.minDateExclusiveNotMet".formatted(contractStartDateField)))
    );
  }

  @Test
  void validate_ContractEndDateBeforeContractStartDate_AssertError() {
    var bidParticipant1 = new InvitationToTenderParticipant(1410);
    var bidParticipant2 = new InvitationToTenderParticipant(1411);
    var bidParticipants = List.of(bidParticipant1, bidParticipant2);
    var countryId = 0;
    var contractEndDateField = AwardedContractForm.CONTRACT_END_DATE_FIELD;
    form.setPreferredBidderId(bidParticipant1.getId());
    form.setPreferredBidderCountryId(countryId);

    form.setContractEndDate(LocalDate.of(1990, 1, 1));

    when(countryService.doesCountryExist(countryId)).thenReturn(true);

    validator.validate(form, bindingResult, new AwardedContractFormValidatorHint(bidParticipants));

    var extractedErrors = ValidatorTestingUtil.extractErrors(bindingResult);

    assertThat(extractedErrors).containsExactly(
        entry("%s.dayInput.inputValue".formatted(contractEndDateField),
            Set.of("%s.dayInput.minDateExclusiveNotMet".formatted(contractEndDateField))),
        entry("%s.monthInput.inputValue".formatted(contractEndDateField),
            Set.of("%s.monthInput.minDateExclusiveNotMet".formatted(contractEndDateField))),
        entry("%s.yearInput.inputValue".formatted(contractEndDateField),
            Set.of("%s.yearInput.minDateExclusiveNotMet".formatted(contractEndDateField)))
    );
  }

  private AwardedContractForm getValidForm() {
    var form = new AwardedContractForm();
    form.setPreferredBidderId(1);
    form.setAwardValue("1.41");
    form.setAwardRationale("test award rationale");
    form.setPreferredBidderCountryId(0);
    form.setContractAwardDate(LocalDate.of(2000, 1, 1));
    form.setPaymentTermsRadio(PaymentTermsRadio.DAYS_30);
    form.setContractStartDate(LocalDate.of(2001, 1, 1));
    form.setContractEndDate(LocalDate.of(2002, 1, 1));
    return form;
  }

}
