package uk.co.nstauthority.scap.scap.summary.actualtender;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static uk.co.nstauthority.scap.utils.ValidatorTestingUtil.bindingResultWithErrors;
import static uk.co.nstauthority.scap.utils.ValidatorTestingUtil.bindingResultWithoutErrors;

import java.util.Collections;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.validation.BindingResult;
import uk.co.nstauthority.scap.scap.actualtender.ActualTender;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivity;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivityBuilder;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivityForm;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivityFormService;
import uk.co.nstauthority.scap.scap.actualtender.activity.ContractStage;
import uk.co.nstauthority.scap.scap.actualtender.activity.InvitationToTenderParticipant;
import uk.co.nstauthority.scap.scap.actualtender.activity.InvitationToTenderParticipantBuilder;
import uk.co.nstauthority.scap.scap.actualtender.activity.awardedcontract.AwardedContract;
import uk.co.nstauthority.scap.scap.actualtender.activity.awardedcontract.AwardedContractBuilder;
import uk.co.nstauthority.scap.scap.actualtender.activity.awardedcontract.AwardedContractForm;
import uk.co.nstauthority.scap.scap.actualtender.activity.awardedcontract.AwardedContractFormService;
import uk.co.nstauthority.scap.scap.actualtender.activity.bidparticipants.BidParticipantsForm;
import uk.co.nstauthority.scap.scap.actualtender.activity.bidparticipants.BidParticipantsFormService;

@ExtendWith(MockitoExtension.class)
class ActualTenderSummaryValidationServiceTest {

  @Mock
  private ActualTenderActivityFormService actualTenderActivityFormService;

  @Mock
  private BidParticipantsFormService bidParticipantsFormService;

  @Mock
  private AwardedContractFormService awardedContractFormService;

  @InjectMocks
  private ActualTenderSummaryValidationService actualTenderSummaryValidationService;

  private ActualTender actualTender;
  private ActualTenderActivity actualTenderActivity;
  private List<InvitationToTenderParticipant> ittParticipants;
  private List<InvitationToTenderParticipant> bidParticipants;
  private AwardedContract awardedContract;

  @BeforeEach
  void setup() {
    actualTender = new ActualTender();
    actualTenderActivity = ActualTenderActivityBuilder.newBuilder()
        .withActualTender(actualTender)
        .build();
    ittParticipants = List.of(
        InvitationToTenderParticipantBuilder.newBuilder()
            .withActualTenderActivity(actualTenderActivity)
            .build(),
        InvitationToTenderParticipantBuilder.newBuilder()
            .withActualTenderActivity(actualTenderActivity)
            .withBidParticipant(true)
            .build()
    );
    bidParticipants = Collections.singletonList(ittParticipants.get(1));
    awardedContract = AwardedContractBuilder.newBuilder()
        .withActualTenderActivity(actualTenderActivity)
        .build();
  }

  @Test
  void isValid_InvalidActualTenderActivity_AssertFalse() {
    var actualTenderActivityForm = new ActualTenderActivityForm();

    when(actualTenderActivityFormService.getForm(actualTenderActivity, ittParticipants))
        .thenReturn(actualTenderActivityForm);
    when(actualTenderActivityFormService.validate(
        eq(actualTenderActivityForm),
        any(BindingResult.class),
        eq(actualTender),
        eq(actualTenderActivity))
    ).thenReturn(bindingResultWithErrors(actualTenderActivityForm));

    var isValid = actualTenderSummaryValidationService
        .isValid(actualTender, actualTenderActivity, ittParticipants, bidParticipants, awardedContract);

    assertFalse(isValid);
    verify(actualTenderActivityFormService).getForm(actualTenderActivity, ittParticipants);
    verify(actualTenderActivityFormService).validate(
        eq(actualTenderActivityForm),
        any(BindingResult.class),
        eq(actualTender),
        eq(actualTenderActivity)
    );
    verifyNoMoreInteractions(
        actualTenderActivityFormService,
        bidParticipantsFormService,
        awardedContractFormService
    );
  }

  @Test
  @DisplayName("Assert is valid when at ITT stage and Actual Tender Activity is valid")
  void isValid_AtIttContractStage_AssertTrue() {
    var actualTenderActivityForm = new ActualTenderActivityForm();

    when(actualTenderActivityFormService.getForm(actualTenderActivity, ittParticipants))
        .thenReturn(actualTenderActivityForm);
    when(actualTenderActivityFormService.validate(
        eq(actualTenderActivityForm),
        any(BindingResult.class),
        eq(actualTender),
        eq(actualTenderActivity))
    ).thenReturn(bindingResultWithoutErrors(actualTenderActivityForm));

    var isValid = actualTenderSummaryValidationService
        .isValid(actualTender, actualTenderActivity, ittParticipants, bidParticipants, awardedContract);

    assertTrue(isValid);
    verify(actualTenderActivityFormService).getForm(actualTenderActivity, ittParticipants);
    verify(actualTenderActivityFormService).validate(
        eq(actualTenderActivityForm),
        any(BindingResult.class),
        eq(actualTender),
        eq(actualTenderActivity)
    );
    verifyNoMoreInteractions(
        actualTenderActivityFormService,
        bidParticipantsFormService,
        awardedContractFormService
    );
  }

  @Test
  @DisplayName("Assert is not valid when at Bid appraisal stage and Bid Participants are invalid")
  void isValid_AtBidAppraisalContractStage_AssertFalse() {
    var actualTenderActivityForm = new ActualTenderActivityForm();
    actualTenderActivity.setContractStage(ContractStage.BID_APPRAISAL);

    when(actualTenderActivityFormService.getForm(actualTenderActivity, ittParticipants))
        .thenReturn(actualTenderActivityForm);
    when(actualTenderActivityFormService.validate(
        eq(actualTenderActivityForm),
        any(BindingResult.class),
        eq(actualTender),
        eq(actualTenderActivity))
    ).thenReturn(bindingResultWithoutErrors(actualTenderActivityForm));

    when(bidParticipantsFormService.validate(
        any(BidParticipantsForm.class),
        any(BindingResult.class),
        eq(bidParticipants))
    ).thenReturn(bindingResultWithErrors(new BidParticipantsForm()));

    var isValid = actualTenderSummaryValidationService
        .isValid(actualTender, actualTenderActivity, ittParticipants, bidParticipants, awardedContract);

    assertFalse(isValid);
    verify(actualTenderActivityFormService).getForm(actualTenderActivity, ittParticipants);
    verify(actualTenderActivityFormService).validate(
        eq(actualTenderActivityForm),
        any(BindingResult.class),
        eq(actualTender),
        eq(actualTenderActivity)
    );
    verify(bidParticipantsFormService).validate(
        any(BidParticipantsForm.class),
        any(BindingResult.class),
        eq(bidParticipants)
    );
    verifyNoMoreInteractions(
        actualTenderActivityFormService,
        bidParticipantsFormService,
        awardedContractFormService
    );
  }

  @Test
  @DisplayName("Assert is valid when at Bid appraisal stage and Bid Participants are valid")
  void isValid_AtBidAppraisalContractStage_AssertTrue() {
    var actualTenderActivityForm = new ActualTenderActivityForm();
    actualTenderActivity.setContractStage(ContractStage.BID_APPRAISAL);

    when(actualTenderActivityFormService.getForm(actualTenderActivity, ittParticipants))
        .thenReturn(actualTenderActivityForm);
    when(actualTenderActivityFormService.validate(
        eq(actualTenderActivityForm),
        any(BindingResult.class),
        eq(actualTender),
        eq(actualTenderActivity))
    ).thenReturn(bindingResultWithoutErrors(actualTenderActivityForm));

    when(bidParticipantsFormService.validate(
        any(BidParticipantsForm.class),
        any(BindingResult.class),
        eq(bidParticipants))
    ).thenReturn(bindingResultWithoutErrors(new BidParticipantsForm()));

    var isValid = actualTenderSummaryValidationService
        .isValid(actualTender, actualTenderActivity, ittParticipants, bidParticipants, awardedContract);

    assertTrue(isValid);
    verify(actualTenderActivityFormService).getForm(actualTenderActivity, ittParticipants);
    verify(actualTenderActivityFormService).validate(
        eq(actualTenderActivityForm),
        any(BindingResult.class),
        eq(actualTender),
        eq(actualTenderActivity)
    );
    verify(bidParticipantsFormService).validate(
        any(BidParticipantsForm.class),
        any(BindingResult.class),
        eq(bidParticipants)
    );
    verifyNoMoreInteractions(
        actualTenderActivityFormService,
        bidParticipantsFormService,
        awardedContractFormService
    );
  }

  @Test
  @DisplayName("Assert not valid when at 'Contract has been awarded' stage, but no awarded contract exists")
  void isValid_WhenAtContractHasBeenAwardedStage_WhenNoAwardedContract_AssertFalse() {
    var actualTenderActivityForm = new ActualTenderActivityForm();
    actualTenderActivity.setContractStage(ContractStage.CONTRACT_AWARDED);

    when(actualTenderActivityFormService.getForm(actualTenderActivity, ittParticipants))
        .thenReturn(actualTenderActivityForm);
    when(actualTenderActivityFormService.validate(
        eq(actualTenderActivityForm),
        any(BindingResult.class),
        eq(actualTender),
        eq(actualTenderActivity))
    ).thenReturn(bindingResultWithoutErrors(actualTenderActivityForm));

    when(bidParticipantsFormService.validate(
        any(BidParticipantsForm.class),
        any(BindingResult.class),
        eq(bidParticipants))
    ).thenReturn(bindingResultWithoutErrors(new BidParticipantsForm()));

    var isValid = actualTenderSummaryValidationService
        .isValid(actualTender, actualTenderActivity, ittParticipants, bidParticipants, null);

    assertFalse(isValid);
    verify(actualTenderActivityFormService).getForm(actualTenderActivity, ittParticipants);
    verify(actualTenderActivityFormService).validate(
        eq(actualTenderActivityForm),
        any(BindingResult.class),
        eq(actualTender),
        eq(actualTenderActivity)
    );
    verify(bidParticipantsFormService).validate(
        any(BidParticipantsForm.class),
        any(BindingResult.class),
        eq(bidParticipants)
    );
    verifyNoMoreInteractions(
        actualTenderActivityFormService,
        bidParticipantsFormService,
        awardedContractFormService
    );
  }

  @Test
  void isValid_WhenAtContractHasBeenAwardedStage_AssertTrue() {
    var actualTenderActivityForm = new ActualTenderActivityForm();
    var awardedContractForm = new AwardedContractForm();
    actualTenderActivity.setContractStage(ContractStage.CONTRACT_AWARDED);

    when(actualTenderActivityFormService.getForm(actualTenderActivity, ittParticipants))
        .thenReturn(actualTenderActivityForm);
    when(actualTenderActivityFormService.validate(
        eq(actualTenderActivityForm),
        any(BindingResult.class),
        eq(actualTender),
        eq(actualTenderActivity))
    ).thenReturn(bindingResultWithoutErrors(actualTenderActivityForm));

    when(bidParticipantsFormService.validate(
        any(BidParticipantsForm.class),
        any(BindingResult.class),
        eq(bidParticipants))
    ).thenReturn(bindingResultWithoutErrors(new BidParticipantsForm()));

    when(awardedContractFormService.getForm(awardedContract)).thenReturn(awardedContractForm);
    when(awardedContractFormService.validate(
        eq(awardedContractForm),
        any(BindingResult.class),
        eq(bidParticipants)
    )).thenReturn(bindingResultWithoutErrors(awardedContractForm));

    var isValid = actualTenderSummaryValidationService
        .isValid(actualTender, actualTenderActivity, ittParticipants, bidParticipants, awardedContract);

    assertTrue(isValid);
    verify(actualTenderActivityFormService).getForm(actualTenderActivity, ittParticipants);
    verify(actualTenderActivityFormService).validate(
        eq(actualTenderActivityForm),
        any(BindingResult.class),
        eq(actualTender),
        eq(actualTenderActivity)
    );
    verify(bidParticipantsFormService).validate(
        any(BidParticipantsForm.class),
        any(BindingResult.class),
        eq(bidParticipants)
    );
    verifyNoMoreInteractions(
        actualTenderActivityFormService,
        bidParticipantsFormService,
        awardedContractFormService
    );
  }

  @Test
  void isValid_WhenAtContractHasBeenAwardedStage_AssertFalse() {
    var actualTenderActivityForm = new ActualTenderActivityForm();
    var awardedContractForm = new AwardedContractForm();
    actualTenderActivity.setContractStage(ContractStage.CONTRACT_AWARDED);

    when(actualTenderActivityFormService.getForm(actualTenderActivity, ittParticipants))
        .thenReturn(actualTenderActivityForm);
    when(actualTenderActivityFormService.validate(
        eq(actualTenderActivityForm),
        any(BindingResult.class),
        eq(actualTender),
        eq(actualTenderActivity))
    ).thenReturn(bindingResultWithoutErrors(actualTenderActivityForm));

    when(bidParticipantsFormService.validate(
        any(BidParticipantsForm.class),
        any(BindingResult.class),
        eq(bidParticipants))
    ).thenReturn(bindingResultWithoutErrors(new BidParticipantsForm()));

    when(awardedContractFormService.getForm(awardedContract)).thenReturn(awardedContractForm);
    when(awardedContractFormService.validate(
        eq(awardedContractForm),
        any(BindingResult.class),
        eq(bidParticipants)
    )).thenReturn(bindingResultWithErrors(awardedContractForm));

    var isValid = actualTenderSummaryValidationService
        .isValid(actualTender, actualTenderActivity, ittParticipants, bidParticipants, awardedContract);

    assertFalse(isValid);
    verify(actualTenderActivityFormService).getForm(actualTenderActivity, ittParticipants);
    verify(actualTenderActivityFormService).validate(
        eq(actualTenderActivityForm),
        any(BindingResult.class),
        eq(actualTender),
        eq(actualTenderActivity)
    );
    verify(bidParticipantsFormService).validate(
        any(BidParticipantsForm.class),
        any(BindingResult.class),
        eq(bidParticipants)
    );
    verifyNoMoreInteractions(
        actualTenderActivityFormService,
        bidParticipantsFormService,
        awardedContractFormService
    );
  }
}
