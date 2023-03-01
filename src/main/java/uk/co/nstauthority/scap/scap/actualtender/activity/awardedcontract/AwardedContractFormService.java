package uk.co.nstauthority.scap.scap.actualtender.activity.awardedcontract;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.BindingResult;
import uk.co.nstauthority.scap.energyportal.CountryService;
import uk.co.nstauthority.scap.scap.actualtender.activity.InvitationToTenderParticipant;

@Service
class AwardedContractFormService {

  public static final String PRESELECTED_LOCATION_REQUEST_PURPOSE = "Pre-fill actual contract award form for SCAP";

  private final AwardedContractFormValidator validator;
  private final CountryService countryService;

  @Autowired
  AwardedContractFormService(AwardedContractFormValidator validator, CountryService countryService) {
    this.validator = validator;
    this.countryService = countryService;
  }

  BindingResult validate(AwardedContractForm form, BindingResult bindingResult,
                         List<InvitationToTenderParticipant> bidParticipants) {
    validator.validate(form, bindingResult, new AwardedContractFormValidatorHint(bidParticipants));
    return bindingResult;
  }

  AwardedContractForm getForm(AwardedContract awardedContract) {
    var form = new AwardedContractForm();
    if (Objects.nonNull(awardedContract.getPreferredBidder())) {
      form.setPreferredBidderId(awardedContract.getPreferredBidder().getId());
    }
    form.setAwardValue(String.valueOf(awardedContract.getAwardValue()));
    form.setAwardRationale(awardedContract.getAwardRationale());
    form.setPreferredBidderCountryId(awardedContract.getPreferredBidderCountryId());
    if (Objects.nonNull(awardedContract.getContractAwardDate())) {
      form.setContractAwardDate(awardedContract.getContractAwardDate());
    }
    form.setPaymentTermsRadio(PaymentTermsRadio.from(awardedContract));
    if (PaymentTermsRadio.OTHER.equals(form.getPaymentTermsRadio())) {
      form.setOtherPaymentTerm(String.valueOf(awardedContract.getPaymentTerms()));
    }
    if (Objects.nonNull(awardedContract.getForecastExecutionStartDate())) {
      form.setContractStartDate(awardedContract.getForecastExecutionStartDate());
    }
    if (Objects.nonNull(awardedContract.getForecastExecutionEndDate())) {
      form.setContractEndDate(awardedContract.getForecastExecutionEndDate());
    }
    return form;
  }

  Optional<Map<String, String>> getPreselectedBidderLocation(Integer countryId) {
    var country = countryService.findCountryById(countryId, PRESELECTED_LOCATION_REQUEST_PURPOSE);
    return country.map(existingCountry ->
        Map.of(String.valueOf(existingCountry.getCountryId()), existingCountry.getCountryName()));
  }

  Optional<Map<String, String>> getPreselectedBidderLocationFromForm(Integer countryId, BindingResult bindingResult) {
    if (bindingResult.hasFieldErrors(AwardedContractFormValidator.BIDDER_LOCATION_FIELD)) {
      return Optional.empty();
    }
    return getPreselectedBidderLocation(countryId);
  }

  static Map<String, String> getSelectOptions(List<InvitationToTenderParticipant> participants) {
    return participants.stream()
        .collect(Collectors.toMap(
            invitationToTenderParticipant -> String.valueOf(invitationToTenderParticipant.getId()),
            InvitationToTenderParticipant::getCompanyName
        ));
  }
}
