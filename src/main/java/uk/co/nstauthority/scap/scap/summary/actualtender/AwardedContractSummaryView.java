package uk.co.nstauthority.scap.scap.summary.actualtender;

import java.math.BigDecimal;

public record AwardedContractSummaryView(String preferredBidderName,
                                         BigDecimal awardValue,
                                         String awardRationale,
                                         String preferredBidderCountry,
                                         String contractAwardDate,
                                         Integer paymentTerms) {
}
