package uk.co.nstauthority.scap.scap.actualtender.summary;

import java.math.BigDecimal;

public record AwardedContractSummaryView(String preferredBidder,
                                         BigDecimal awardValue,
                                         String awardRationale,
                                         String preferredBidderLocation) {
}
