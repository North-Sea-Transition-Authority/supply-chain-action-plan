package uk.co.nstauthority.scap.scap.actualtender.activity.awardedcontract;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.Arrays;
import java.util.Objects;
import java.util.stream.Collectors;
import org.jetbrains.annotations.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.Errors;
import org.springframework.validation.SmartValidator;
import org.springframework.validation.ValidationUtils;
import uk.co.fivium.formlibrary.validator.date.ThreeFieldDateInputValidator;
import uk.co.fivium.formlibrary.validator.decimal.DecimalInputValidator;
import uk.co.fivium.formlibrary.validator.integer.IntegerInputValidator;
import uk.co.fivium.formlibrary.validator.string.StringInputValidator;
import uk.co.nstauthority.scap.energyportal.CountryService;
import uk.co.nstauthority.scap.scap.actualtender.activity.InvitationToTenderParticipant;
import uk.co.nstauthority.scap.util.ValidationUtil;

@Service
class AwardedContractFormValidator implements SmartValidator {

  static final String BIDDER_LOCATION_FIELD = "preferredBidderCountryId";
  static final String PREFERRED_BIDDER_FIELD = "preferredBidderId";
  static final String PAYMENT_TERMS_RADIO_FIELD = "paymentTermsRadio";

  private final CountryService countryService;

  @Autowired
  AwardedContractFormValidator(CountryService countryService) {
    this.countryService = countryService;
  }

  @Override
  public boolean supports(@NotNull Class<?> clazz) {
    return AwardedContractForm.class.equals(clazz);
  }

  @Override
  public void validate(@NotNull Object target, @NotNull Errors errors) {
    throw new IllegalStateException("Missing 3rd argument of type List<InvitationToTenderParticipant>");
  }

  @Override
  public void validate(@NotNull Object target, @NotNull Errors errors, @NotNull Object... validationHints) {
    var awardedContractFormValidatorHint = Arrays.stream(validationHints)
        .filter(Objects::nonNull)
        .filter(validationHint -> AwardedContractFormValidatorHint.class.equals(validationHint.getClass()))
        .map(AwardedContractFormValidatorHint.class::cast)
        .findFirst()
        .orElseThrow(() -> new IllegalStateException("Cannot get AwardedContractFormValidatorHint"));

    var form = (AwardedContractForm) target;

    ValidationUtils.rejectIfEmpty(
        errors,
        PREFERRED_BIDDER_FIELD,
        String.format("%s.required", PREFERRED_BIDDER_FIELD),
        "Select a preferred bidder");
    if (!errors.hasFieldErrors(PREFERRED_BIDDER_FIELD)) {
      var permittedBidderIds = awardedContractFormValidatorHint.bidParticipants().stream()
          .map(InvitationToTenderParticipant::getId)
          .collect(Collectors.toSet());
      if (!permittedBidderIds.contains(form.getPreferredBidderId())) {
        errors.rejectValue(
            PREFERRED_BIDDER_FIELD,
            String.format("%s.doesNotExist", PREFERRED_BIDDER_FIELD),
            "Select a valid preferred bidder");
      }
    }

    DecimalInputValidator.builder()
        .mustBeMoreThanOrEqualTo(BigDecimal.valueOf(0.001))
        .mustHaveNoMoreThanDecimalPlaces(3)
        .validate(form.getAwardValue(), errors);

    StringInputValidator.builder()
        .mustHaveCharacterCountAtMost(ValidationUtil.TEXT_AREA_STANDARD_LIMIT)
        .validate(form.getAwardRationale(), errors);

    ValidationUtils.rejectIfEmpty(
        errors,
        BIDDER_LOCATION_FIELD,
        String.format("%s.required", BIDDER_LOCATION_FIELD),
        "Select the location of the preferred bidder");
    if (!errors.hasFieldErrors(BIDDER_LOCATION_FIELD)
        && !countryService.doesCountryExist(form.getPreferredBidderCountryId())) {
      errors.rejectValue(
          BIDDER_LOCATION_FIELD,
          String.format("%s.doesNotExist", BIDDER_LOCATION_FIELD),
          "Select a valid location of the preferred bidder"
      );
    }

    ThreeFieldDateInputValidator.builder()
        .mustBeBeforeOrEqualTo(LocalDate.now())
        .validate(form.getContractAwardDate(), errors);


    ValidationUtils.rejectIfEmpty(
        errors,
        PAYMENT_TERMS_RADIO_FIELD,
        "%s.required".formatted(PAYMENT_TERMS_RADIO_FIELD),
        "Select the payment terms"
    );

    if (PaymentTermsRadio.OTHER.equals(form.getPaymentTermsRadio())) {
      IntegerInputValidator.builder()
          .mustBeMoreThanOrEqualTo(0)
          .validate(form.getOtherPaymentTerm(), errors);
    }

    // Contract start date validation
    var contractStartValidator = ThreeFieldDateInputValidator.builder();

    var contractAwardDateOpt = form.getContractAwardDate().getAsLocalDate();
    contractAwardDateOpt.ifPresent(contractStartValidator::mustBeAfterDate);

    contractStartValidator.validate(form.getContractStartDate(), errors);

    // Contract end date validation
    var contractEndDateValidator = ThreeFieldDateInputValidator.builder();

    var contractStartDateOpt = form.getContractStartDate().getAsLocalDate();
    contractStartDateOpt.ifPresent(contractEndDateValidator::mustBeAfterDate);

    contractEndDateValidator.validate(form.getContractEndDate(), errors);
  }
}
