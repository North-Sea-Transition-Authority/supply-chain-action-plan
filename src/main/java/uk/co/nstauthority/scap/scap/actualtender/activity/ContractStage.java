package uk.co.nstauthority.scap.scap.actualtender.activity;

import java.util.Map;
import uk.co.nstauthority.scap.enumutil.Displayable;
import uk.co.nstauthority.scap.enumutil.DisplayableEnumOptionUtil;

public enum ContractStage implements Displayable {
  INVITATION_TO_TENDER_IS_LIVE("Invitation to tender is live", 10),
  BID_APPRAISAL("Bid appraisal", 20),
  CONTRACT_AWARDED("Contract has been awarded", 30);


  private final String displayName;
  private final Integer displayOrder;

  ContractStage(String displayName, Integer displayOrder) {
    this.displayName = displayName;
    this.displayOrder = displayOrder;
  }

  public static Map<String, String> getContractStages() {
    return DisplayableEnumOptionUtil.getDisplayableOptions(ContractStage.class);
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
}
