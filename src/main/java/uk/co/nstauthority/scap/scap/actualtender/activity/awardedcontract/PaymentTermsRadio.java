package uk.co.nstauthority.scap.scap.actualtender.activity.awardedcontract;

import java.util.Map;
import java.util.Objects;
import uk.co.nstauthority.scap.enumutil.Displayable;
import uk.co.nstauthority.scap.enumutil.DisplayableEnumOptionUtil;

enum PaymentTermsRadio implements Displayable {
  DAYS_30("30 days", 1, 30),
  DAYS_60("60 days", 2, 60),
  OTHER("Other", 3, 0);

  private final String displayName;
  private final int displayOrder;
  private final int paymentTerm;

  PaymentTermsRadio(String displayName, int displayOrder, int paymentTerm) {
    this.displayName = displayName;
    this.displayOrder = displayOrder;
    this.paymentTerm = paymentTerm;
  }

  static PaymentTermsRadio from(AwardedContract awardedContract) {
    if (Objects.isNull(awardedContract.getPaymentTerms())) {
      return null;
    }
    return switch (awardedContract.getPaymentTerms()) {
      case 30 -> DAYS_30;
      case 60 -> DAYS_60;
      default -> OTHER;
    };
  }

  static Map<String, String> getRadioOptions() {
    return DisplayableEnumOptionUtil.getDisplayableOptions(PaymentTermsRadio.class);
  }

  @Override
  public String getDisplayName() {
    return this.displayName;
  }

  @Override
  public int getDisplayOrder() {
    return this.displayOrder;
  }

  @Override
  public String getEnumName() {
    return this.name();
  }

  public int getPaymentTerm() {
    return paymentTerm;
  }
}
