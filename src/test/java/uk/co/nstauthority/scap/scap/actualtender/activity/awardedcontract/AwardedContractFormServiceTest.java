package uk.co.nstauthority.scap.scap.actualtender.activity.awardedcontract;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.math.BigDecimal;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
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

  @Test
  void getForm() {
    var awardedContract = new AwardedContract(141);
    var preferredBidderId = 1141;
    var preferredBidderLocation = 2141;
    var awardValue = BigDecimal.valueOf(3.14);
    var awardRationale = "Test award rationale";
    awardedContract.setPreferredBidder(new InvitationToTenderParticipant(preferredBidderId));
    awardedContract.setAwardValue(awardValue);
    awardedContract.setAwardRationale(awardRationale);
    awardedContract.setPreferredBidderLocation(preferredBidderLocation);

    var form = awardedContractFormService.getForm(awardedContract);

    assertThat(form).extracting(
        AwardedContractForm::getPreferredBidderId,
        actualForm -> actualForm.getAwardValue().getInputValue(),
        actualForm -> actualForm.getAwardRationale().getInputValue(),
        AwardedContractForm::getPreferredBidderLocation
    ).containsExactly(
        preferredBidderId,
        String.valueOf(awardValue),
        awardRationale,
        preferredBidderLocation
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
        new FieldError("form", "preferredBidderLocation", "Test message")
    );

    var returnedCountry = awardedContractFormService
        .getPreselectedBidderLocationFromForm(countryId, bindingResultWithErrors);

    assertThat(returnedCountry).isEmpty();
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
