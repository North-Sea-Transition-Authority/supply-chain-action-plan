package uk.co.nstauthority.scap.scap.actualtender.activity.awardedcontract;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Stream;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.validation.BeanPropertyBindingResult;
import org.springframework.validation.FieldError;
import uk.co.fivium.energyportalapi.generated.types.Country;
import uk.co.nstauthority.scap.energyportal.CountryService;
import uk.co.nstauthority.scap.scap.actualtender.activity.InvitationToTenderParticipant;

@ExtendWith(MockitoExtension.class)
class AwardedContractFormServiceTest {

  @Mock
  AwardedContractFormValidator validator;

  @Mock
  CountryService countryService;

  @InjectMocks
  AwardedContractFormService awardedContractFormService;

  @Test
  void validate_VerifyCallsValidator() {
    var form = new AwardedContractForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");
    var bidParticipants = List.of(new InvitationToTenderParticipant(1));
    var argumentCaptor = ArgumentCaptor.forClass(AwardedContractFormValidatorHint.class);

    awardedContractFormService.validate(form, bindingResult, bidParticipants);

    verify(validator).validate(eq(form), eq(bindingResult), argumentCaptor.capture());
    assertThat(argumentCaptor.getValue().bidParticipants()).isEqualTo(bidParticipants);
  }

  static Stream<Arguments> paymentTermsArguments() {
    var otherPaymentTerm = 31;
    return Stream.of(
        Arguments.of(null, null, null),
        Arguments.of(otherPaymentTerm, PaymentTermsRadio.OTHER, String.valueOf(otherPaymentTerm)),
        Arguments.of(PaymentTermsRadio.DAYS_30.getPaymentTerm(), PaymentTermsRadio.DAYS_30, null),
        Arguments.of(PaymentTermsRadio.DAYS_60.getPaymentTerm(), PaymentTermsRadio.DAYS_60, null)
    );
  }

  @ParameterizedTest
  @MethodSource("paymentTermsArguments")
  void getForm(
      Integer awardedContractPaymentTerms,
      PaymentTermsRadio expectedFormRadio,
      String expectFormOtherPaymentTerm) {
    var awardedContract = new AwardedContract(141);
    var preferredBidderId = 1141;
    var preferredBidderLocation = 2141;
    var awardValue = BigDecimal.valueOf(3.14);
    var awardRationale = "Test award rationale";
    var awardDate = LocalDate.of(2000, 1, 1);
    awardedContract.setPreferredBidder(new InvitationToTenderParticipant(preferredBidderId));
    awardedContract.setAwardValue(awardValue);
    awardedContract.setAwardRationale(awardRationale);
    awardedContract.setPreferredBidderCountryId(preferredBidderLocation);
    awardedContract.setContractAwardDate(awardDate);
    awardedContract.setPaymentTerms(awardedContractPaymentTerms);

    var form = awardedContractFormService.getForm(awardedContract);

    assertThat(form).extracting(
        AwardedContractForm::getPreferredBidderId,
        actualForm -> actualForm.getAwardValue().getInputValue(),
        actualForm -> actualForm.getAwardRationale().getInputValue(),
        AwardedContractForm::getPreferredBidderCountryId,
        actualForm -> actualForm.getContractAwardDate().getAsLocalDate(),
        AwardedContractForm::getPaymentTermsRadio,
        actualForm -> actualForm.getOtherPaymentTerm().getInputValue()
    ).containsExactly(
        preferredBidderId,
        String.valueOf(awardValue),
        awardRationale,
        preferredBidderLocation,
        Optional.of(awardDate),
        expectedFormRadio,
        expectFormOtherPaymentTerm
    );
  }

  @Test
  void getPreselectedBidderLocation_NonExistentCountry_AssertEmpty() {
    var countryId = 9999;

    when(countryService.findCountryById(countryId, AwardedContractFormService.PRESELECTED_LOCATION_REQUEST_PURPOSE))
        .thenReturn(Optional.empty());

    var returnedCountry = awardedContractFormService.getPreselectedBidderLocation(countryId);

    assertThat(returnedCountry).isEmpty();
  }

  @Test
  void getPreselectedBidderLocation() {
    var countryId = 0;
    var country = new Country(countryId, "United Kingdom", null, null);

    when(countryService.findCountryById(countryId, AwardedContractFormService.PRESELECTED_LOCATION_REQUEST_PURPOSE))
        .thenReturn(Optional.of(country));

    var returnedCountry = awardedContractFormService.getPreselectedBidderLocation(countryId);

    assertThat(returnedCountry).contains(
        Map.of(String.valueOf(countryId), country.getCountryName())
    );
  }

  @Test
  void getPreselectedBidderLocationFromForm_HasErrors_AssertEmpty() {
    var countryId = 0;
    var bindingResultWithErrors = new BeanPropertyBindingResult(new AwardedContractForm(), "form");
    bindingResultWithErrors.addError(
        new FieldError("form", AwardedContractFormValidator.BIDDER_LOCATION_FIELD, "Test message")
    );

    var returnedCountry = awardedContractFormService
        .getPreselectedBidderLocationFromForm(countryId, bindingResultWithErrors);

    assertThat(returnedCountry).isEmpty();

    verifyNoInteractions(countryService);
  }

  @Test
  void getPreselectedBidderLocationFromForm_NoErrors() {
    var countryId = 0;
    var country = new Country(countryId, "United Kingdom", null, null);
    var bindingResultWithoutErrors = new BeanPropertyBindingResult(new AwardedContractForm(), "form");

    when(countryService.findCountryById(countryId, AwardedContractFormService.PRESELECTED_LOCATION_REQUEST_PURPOSE))
        .thenReturn(Optional.of(country));

    var returnedCountry = awardedContractFormService
        .getPreselectedBidderLocationFromForm(countryId, bindingResultWithoutErrors);

    assertThat(returnedCountry).contains(
        Map.of(String.valueOf(countryId), country.getCountryName())
    );
  }
}
