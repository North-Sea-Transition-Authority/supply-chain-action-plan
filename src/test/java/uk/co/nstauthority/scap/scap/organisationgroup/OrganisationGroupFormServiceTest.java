package uk.co.nstauthority.scap.scap.organisationgroup;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Optional;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.validation.BeanPropertyBindingResult;
import org.springframework.validation.BindingResult;
import uk.co.fivium.energyportalapi.generated.types.OrganisationGroup;
import uk.co.nstauthority.scap.error.exception.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.fds.searchselector.RestSearchItem;
import uk.co.nstauthority.scap.scap.detail.ScapDetailEntityTestUtil;
import uk.co.nstauthority.scap.scap.scap.ScapEntityTestUtil;
import uk.co.nstauthority.scap.scap.scap.ScapId;

@ExtendWith(MockitoExtension.class)
class OrganisationGroupFormServiceTest {

  @Mock
  private OrganisationGroupFormValidator organisationGroupFormValidator;

  @Mock
  private OrganisationGroupService organisationGroupService;

  @InjectMocks
  private OrganisationGroupFormService organisationGroupFormService;

  @BeforeEach
  void setup() {
    organisationGroupFormService = spy(organisationGroupFormService);
  }

  @Test
  void validate_verifyMethodCall() {
    var form = new OrganisationGroupForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");

    organisationGroupFormService.validate(form, bindingResult);

    verify(organisationGroupFormValidator).validate(form, bindingResult);
  }

  @Test
  void getForm() {
    var orgGroupId = 5;
    var scap = ScapEntityTestUtil.scapBuilder()
        .withOrganisationGroupId(orgGroupId)
        .build();
    var scapDetail = ScapDetailEntityTestUtil.scapDetailBuilder()
        .withScap(scap)
        .withTierOneContractor(false)
        .build();

    var form = organisationGroupFormService.getForm(scapDetail);

    assertThat(form).extracting(
        OrganisationGroupForm::getOrganisationGroupId,
        OrganisationGroupForm::getIsTierOneContractor,
        OrganisationGroupForm::getParentScapId
    ).containsExactly(
        orgGroupId,
        false,
        null
    );
  }

  @Test
  void getForm_IsTierOneContractor() {
    var orgGroupId = 5;
    var scap = ScapEntityTestUtil.scapBuilder()
        .withOrganisationGroupId(orgGroupId)
        .build();
    var parentScap = ScapEntityTestUtil.scapBuilder()
        .withScapId(ScapId.valueOf(43465))
        .build();
    var scapDetail = ScapDetailEntityTestUtil.scapDetailBuilder()
        .withScap(scap)
        .withTierOneContractor(true)
        .withParentScap(parentScap)
        .build();

    var form = organisationGroupFormService.getForm(scapDetail);

    assertThat(form).extracting(
        OrganisationGroupForm::getOrganisationGroupId,
        OrganisationGroupForm::getIsTierOneContractor,
        OrganisationGroupForm::getParentScapId
    ).containsExactly(
        orgGroupId,
        true,
        parentScap.getId()
    );
  }

  @Test
  void getPreselectedScap_NotTierOneContractor_AssertNull() {
    var scapDetail = ScapDetailEntityTestUtil.scapDetailBuilder()
        .withTierOneContractor(false)
        .build();

    var preselectedScap = organisationGroupFormService.getPreselectedScap(scapDetail);

    assertThat(preselectedScap).isNull();
  }

  @Test
  void getPreselectedScap_IsTierOneContractor_ParentScapNull_AssertNull() {
    var scapDetail = ScapDetailEntityTestUtil.scapDetailBuilder()
        .withTierOneContractor(true)
        .withParentScap(null)
        .build();

    var preselectedScap = organisationGroupFormService.getPreselectedScap(scapDetail);

    assertThat(preselectedScap).isNull();
  }

  @Test
  void getPreselectedScap_IsTierOneContractor() {
    var parentOperatorId = 55;
    var parentScap = ScapEntityTestUtil.scapBuilder()
        .withOrganisationGroupId(parentOperatorId)
        .withReference("SCAP/2022/1")
        .build();
    var parentOperator = OrganisationGroup.newBuilder()
        .organisationGroupId(parentOperatorId)
        .name("CENTRICA")
        .build();
    var scapDetail = ScapDetailEntityTestUtil.scapDetailBuilder()
        .withTierOneContractor(true)
        .withParentScap(parentScap)
        .build();

    when(organisationGroupService.getOrganisationGroupById(
            parentScap.getOrganisationGroupId(),
            OrganisationGroupFormService.ORG_GROUP_REQUEST_PURPOSE
    )).thenReturn(Optional.of(parentOperator));

    var preselectedScap = organisationGroupFormService.getPreselectedScap(scapDetail);

    assertThat(preselectedScap).extracting(
        RestSearchItem::id,
        RestSearchItem::text
    ).containsExactly(
        String.valueOf(parentScap.getId()),
        parentScap.getReference() + " - " + parentOperator.getName()
    );
  }

  @Test
  void getPreselectedScap_WithBindingResult_NoErrors() {
    var scapDetail = ScapDetailEntityTestUtil.scapDetailBuilder()
        .withTierOneContractor(false)
        .build();
    var bindingResult = mock(BindingResult.class);
    doReturn(false).when(bindingResult).hasFieldErrors(OrganisationGroupFormValidator.SCAP_ID_FIELD_NAME);

    var preselectedScap = organisationGroupFormService.getPreselectedScap(scapDetail, bindingResult);

    assertThat(preselectedScap).isNull();
    verify(organisationGroupFormService).getPreselectedScap(scapDetail);
  }

  @Test
  void getPreselectedScap_WithBindingResult_WithErrors() {
    var scapDetail = ScapDetailEntityTestUtil.scapDetailBuilder()
        .withTierOneContractor(false)
        .build();
    var bindingResult = mock(BindingResult.class);
    doReturn(true).when(bindingResult).hasFieldErrors(OrganisationGroupFormValidator.SCAP_ID_FIELD_NAME);

    var preselectedScap = organisationGroupFormService.getPreselectedScap(scapDetail, bindingResult);

    assertThat(preselectedScap).isNull();
    verify(organisationGroupFormService, never()).getPreselectedScap(scapDetail);
  }

  @Test
  void getPreselectedScap_OrgGroupNotFound_AssertThrows() {
    var parentOperatorId = 55;
    var parentScap = ScapEntityTestUtil.scapBuilder()
        .withOrganisationGroupId(parentOperatorId)
        .withReference("SCAP/2022/1")
        .build();

    var scapDetail = ScapDetailEntityTestUtil.scapDetailBuilder()
        .withTierOneContractor(true)
        .withParentScap(parentScap)
        .build();
    var bindingResult = mock(BindingResult.class);
    doReturn(false).when(bindingResult).hasFieldErrors(OrganisationGroupFormValidator.SCAP_ID_FIELD_NAME);

    assertThatThrownBy(() -> organisationGroupFormService.getPreselectedScap(scapDetail, bindingResult))
        .isInstanceOf(ScapEntityNotFoundException.class)
        .hasMessage("Could not find org group with ID [%d]".formatted(parentOperatorId));
  }
}
