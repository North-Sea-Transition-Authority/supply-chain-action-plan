package uk.co.nstauthority.scap.scap.scap;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.Collections;
import java.util.Optional;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.validation.BindingResult;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.actualtender.ActualTenderTaskListItemService;
import uk.co.nstauthority.scap.scap.actualtender.hasactualtender.HasActualTenderController;
import uk.co.nstauthority.scap.scap.contractingperformance.ContractingPerformanceTaskListItemService;
import uk.co.nstauthority.scap.scap.contractingperformance.hascontractingperformance.HasContractingPerformanceController;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.detail.ScapDetailEntityTestUtil;
import uk.co.nstauthority.scap.scap.organisationgroup.OrganisationGroupController;
import uk.co.nstauthority.scap.scap.organisationgroup.OrganisationGroupForm;
import uk.co.nstauthority.scap.scap.organisationgroup.OrganisationGroupFormService;
import uk.co.nstauthority.scap.scap.plannedtender.PlannedTenderTaskListItemService;
import uk.co.nstauthority.scap.scap.plannedtender.hasplannedtender.HasPlannedTenderController;
import uk.co.nstauthority.scap.scap.projectdetails.ProjectDetails;
import uk.co.nstauthority.scap.scap.projectdetails.ProjectDetailsController;
import uk.co.nstauthority.scap.scap.projectdetails.ProjectDetailsForm;
import uk.co.nstauthority.scap.scap.projectdetails.ProjectDetailsFormService;
import uk.co.nstauthority.scap.scap.projectdetails.ProjectDetailsService;
import uk.co.nstauthority.scap.scap.projectperformance.ProjectPerformance;
import uk.co.nstauthority.scap.scap.projectperformance.ProjectPerformanceController;
import uk.co.nstauthority.scap.scap.projectperformance.ProjectPerformanceForm;
import uk.co.nstauthority.scap.scap.projectperformance.ProjectPerformanceFormService;
import uk.co.nstauthority.scap.scap.projectperformance.ProjectPerformanceService;
import uk.co.nstauthority.scap.tasklist.TaskListItem;
import uk.co.nstauthority.scap.tasklist.TaskListLabel;
import uk.co.nstauthority.scap.tasklist.TaskListSection;
import uk.co.nstauthority.scap.utils.ValidatorTestingUtil;

@ExtendWith(MockitoExtension.class)
class ScapFormTaskListSectionServiceTest {


  @Mock
  private ProjectDetailsService projectDetailsService;

  @Mock
  private ProjectDetailsFormService projectDetailsFormService;

  @Mock
  private PlannedTenderTaskListItemService plannedTenderTaskListItemService;

  @Mock
  private ActualTenderTaskListItemService actualTenderTaskListItemService;

  @Mock
  private ContractingPerformanceTaskListItemService contractingPerformanceTaskListItemService;

  @Mock
  private ProjectPerformanceService projectPerformanceService;

  @Mock
  private ProjectPerformanceFormService projectPerformanceFormService;

  @Mock
  private OrganisationGroupFormService organisationGroupFormService;

  @InjectMocks
  private ScapFormTaskListSectionService scapFormTaskListSectionService;

  private static final ScapId SCAP_ID = new ScapId(242);
  private static final Scap SCAP = ScapEntityTestUtil.scapBuilder()
      .withScapId(SCAP_ID)
      .build();
  private static final ScapDetail SCAP_DETAIL = ScapDetailEntityTestUtil.scapDetailBuilder()
      .withScap(SCAP)
      .build();

  @BeforeEach
  void setup() {
    scapFormTaskListSectionService = spy(scapFormTaskListSectionService);
  }

  @Test
  void getSection() {
    var testTaskListItem = new TaskListItem("test", TaskListLabel.COMPLETED, "#");
    doReturn(testTaskListItem).when(scapFormTaskListSectionService)
        .getScapOperatorTaskListItem(SCAP_ID, SCAP_DETAIL);
    doReturn(testTaskListItem).when(scapFormTaskListSectionService)
        .getProjectDetailsTaskListItem(SCAP_ID, SCAP_DETAIL);
    doReturn(testTaskListItem).when(scapFormTaskListSectionService)
        .getPlannedTenderTaskListItem(SCAP_ID, SCAP_DETAIL);
    doReturn(testTaskListItem).when(scapFormTaskListSectionService)
        .getActualTenderTaskListItem(SCAP_ID, SCAP_DETAIL);
    doReturn(testTaskListItem).when(scapFormTaskListSectionService)
        .getContractingPerformanceTaskListItem(SCAP_ID, SCAP_DETAIL);
    doReturn(testTaskListItem).when(scapFormTaskListSectionService)
        .getProjectPerformanceTaskListItem(SCAP_ID, SCAP_DETAIL);

    var section = scapFormTaskListSectionService.getSection(SCAP_DETAIL);

    assertThat(section).isNotEmpty();
    assertThat(section.get()).extracting(
        TaskListSection::displayName,
        TaskListSection::displayOrder
    ).containsExactly(
        ScapFormTaskListSectionService.DISPLAY_NAME,
        ScapFormTaskListSectionService.DISPLAY_ORDER
    );
    assertThat(section.get().items()).hasSize(6);
  }

  @Test
  void getScapOperatorTaskListItem_NotComplete() {
    var form = new OrganisationGroupForm();
    var bindingResult = ValidatorTestingUtil.bindingResultWithErrors(form);

    when(organisationGroupFormService.getForm(SCAP_DETAIL)).thenReturn(form);
    when(organisationGroupFormService.validate(eq(form), any(BindingResult.class))).thenReturn(bindingResult);

    var label = scapFormTaskListSectionService.getScapOperatorTaskListItem(SCAP_ID, SCAP_DETAIL);

    assertThat(label).extracting(
        TaskListItem::label,
        TaskListItem::displayName,
        TaskListItem::actionUrl
    ).containsExactly(
        TaskListLabel.NOT_COMPLETED,
        ScapFormTaskListSectionService.OPERATOR_DISPLAY_NAME,
        ReverseRouter.route(on(OrganisationGroupController.class).renderExistingScapOrganisationGroupForm(SCAP_ID))
    );
  }

  @Test
  void getScapOperatorTaskListItem_Complete() {
    var form = new OrganisationGroupForm();
    var bindingResult = ValidatorTestingUtil.bindingResultWithoutErrors(form);

    when(organisationGroupFormService.getForm(SCAP_DETAIL)).thenReturn(form);
    when(organisationGroupFormService.validate(eq(form), any(BindingResult.class))).thenReturn(bindingResult);

    var label = scapFormTaskListSectionService.getScapOperatorTaskListItem(SCAP_ID, SCAP_DETAIL);

    assertThat(label).extracting(
        TaskListItem::label,
        TaskListItem::displayName,
        TaskListItem::actionUrl
    ).containsExactly(
        TaskListLabel.COMPLETED,
        ScapFormTaskListSectionService.OPERATOR_DISPLAY_NAME,
        ReverseRouter.route(on(OrganisationGroupController.class).renderExistingScapOrganisationGroupForm(SCAP_ID))
    );
  }

  @Test
  void getProjectDetailsTaskListItem_NoProjectDetails() {
    when(projectDetailsService.findByScapDetail(SCAP_DETAIL)).thenReturn(Optional.empty());

    var label = scapFormTaskListSectionService.getProjectDetailsTaskListItem(SCAP_ID, SCAP_DETAIL);

    assertThat(label).extracting(
        TaskListItem::label,
        TaskListItem::displayName,
        TaskListItem::actionUrl
    ).containsExactly(
        TaskListLabel.NOT_COMPLETED,
        ScapFormTaskListSectionService.PROJECT_DETAILS_DISPLAY_NAME,
        ReverseRouter.route(on(ProjectDetailsController.class).renderProjectDetailsForm(SCAP_ID))
    );
  }

  @Test
  void getProjectDetailsTaskListItem_ProjectDetailsInvalid() {
    var projectDetails = new ProjectDetails();
    var form = new ProjectDetailsForm();

    when(projectDetailsService.findByScapDetail(SCAP_DETAIL)).thenReturn(Optional.of(projectDetails));
    when(projectDetailsService.getProjectFields(projectDetails)).thenReturn(Collections.emptyList());
    when(projectDetailsService.getProjectFacilities(projectDetails)).thenReturn(Collections.emptyList());
    when(projectDetailsFormService.getForm(projectDetails, Collections.emptySet(), Collections.emptySet()))
        .thenReturn(form);
    when(projectDetailsFormService.validate(eq(form), any(BindingResult.class)))
        .thenReturn(ValidatorTestingUtil.bindingResultWithErrors(form));

    var label = scapFormTaskListSectionService.getProjectDetailsTaskListItem(SCAP_ID, SCAP_DETAIL);

    assertThat(label).extracting(
        TaskListItem::label,
        TaskListItem::displayName,
        TaskListItem::actionUrl
    ).containsExactly(
        TaskListLabel.NOT_COMPLETED,
        ScapFormTaskListSectionService.PROJECT_DETAILS_DISPLAY_NAME,
        ReverseRouter.route(on(ProjectDetailsController.class).renderProjectDetailsForm(SCAP_ID))
    );
  }

  @Test
  void getProjectDetailsTaskListItem_ValidProjectDetails() {
    var projectDetails = new ProjectDetails();
    var form = new ProjectDetailsForm();

    when(projectDetailsService.findByScapDetail(SCAP_DETAIL)).thenReturn(Optional.of(projectDetails));
    when(projectDetailsService.getProjectFields(projectDetails)).thenReturn(Collections.emptyList());
    when(projectDetailsService.getProjectFacilities(projectDetails)).thenReturn(Collections.emptyList());
    when(projectDetailsFormService.getForm(projectDetails, Collections.emptySet(), Collections.emptySet()))
        .thenReturn(form);
    when(projectDetailsFormService.validate(eq(form), any(BindingResult.class)))
        .thenReturn(ValidatorTestingUtil.bindingResultWithoutErrors(form));

    var label = scapFormTaskListSectionService.getProjectDetailsTaskListItem(SCAP_ID, SCAP_DETAIL);

    assertThat(label).extracting(
        TaskListItem::label,
        TaskListItem::displayName,
        TaskListItem::actionUrl
    ).containsExactly(
        TaskListLabel.COMPLETED,
        ScapFormTaskListSectionService.PROJECT_DETAILS_DISPLAY_NAME,
        ReverseRouter.route(on(ProjectDetailsController.class).renderProjectDetailsForm(SCAP_ID))
    );
  }

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void getPlannedTenderTaskListItem(boolean isValid) {
    when(plannedTenderTaskListItemService.isValid(SCAP_DETAIL)).thenReturn(isValid);

    var label = scapFormTaskListSectionService.getPlannedTenderTaskListItem(SCAP_ID, SCAP_DETAIL);

    assertThat(label).extracting(
        TaskListItem::label,
        TaskListItem::displayName,
        TaskListItem::actionUrl
    ).containsExactly(
        TaskListLabel.from(isValid),
        ScapFormTaskListSectionService.PLANNED_TENDER_DISPLAY_NAME,
        ReverseRouter.route(on(HasPlannedTenderController.class).renderHasPlannedTenderActivityForm(SCAP_ID))
    );
  }

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void getActualTenderTaskListItem(boolean isValid) {
    when(actualTenderTaskListItemService.isValid(SCAP_DETAIL)).thenReturn(isValid);

    var label = scapFormTaskListSectionService.getActualTenderTaskListItem(SCAP_ID, SCAP_DETAIL);

    assertThat(label).extracting(
        TaskListItem::label,
        TaskListItem::displayName,
        TaskListItem::actionUrl
    ).containsExactly(
        TaskListLabel.from(isValid),
        ScapFormTaskListSectionService.ACTUAL_TENDER_DISPLAY_NAME,
        ReverseRouter.route(on(HasActualTenderController.class).renderHasActualTenderForm(SCAP_ID))
    );
  }

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void getContractingPerformanceTaskListItem(boolean isValid) {
    when(contractingPerformanceTaskListItemService.isValid(SCAP_DETAIL)).thenReturn(isValid);

    var label = scapFormTaskListSectionService.getContractingPerformanceTaskListItem(SCAP_ID, SCAP_DETAIL);

    assertThat(label).extracting(
        TaskListItem::label,
        TaskListItem::displayName,
        TaskListItem::actionUrl
    ).containsExactly(
        TaskListLabel.from(isValid),
        ScapFormTaskListSectionService.CONTRACTING_PERFORMANCE_DISPLAY_NAME,
        ReverseRouter.route(on(HasContractingPerformanceController.class).renderHasContractingPerformanceForm(SCAP_ID))
    );
  }

  @Test
  void getProjectPerformanceTaskListItem_NoProjectPerformance() {
    when(projectPerformanceService.findByScapDetail(SCAP_DETAIL)).thenReturn(Optional.empty());

    var label = scapFormTaskListSectionService.getProjectPerformanceTaskListItem(SCAP_ID, SCAP_DETAIL);

    assertThat(label).extracting(
        TaskListItem::label,
        TaskListItem::displayName,
        TaskListItem::actionUrl
    ).containsExactly(
        TaskListLabel.NOT_COMPLETED,
        ScapFormTaskListSectionService.PROJECT_PERFORMANCE_DISPLAY_NAME,
        ReverseRouter.route(on(ProjectPerformanceController.class).renderProjectPerformanceForm(SCAP_ID))
    );
  }

  @Test
  void getProjectPerformanceTaskListItem_InvalidProjectPerformance() {
    var projectPerformance = new ProjectPerformance();
    var form = new ProjectPerformanceForm();
    var bindingResult = ValidatorTestingUtil.bindingResultWithErrors(form);

    when(projectPerformanceService.findByScapDetail(SCAP_DETAIL)).thenReturn(Optional.of(projectPerformance));
    when(projectPerformanceFormService.getForm(projectPerformance)).thenReturn(form);
    when(projectPerformanceFormService.validate(eq(form), any(BindingResult.class))).thenReturn(bindingResult);

    var label = scapFormTaskListSectionService.getProjectPerformanceTaskListItem(SCAP_ID, SCAP_DETAIL);

    assertThat(label).extracting(
        TaskListItem::label,
        TaskListItem::displayName,
        TaskListItem::actionUrl
    ).containsExactly(
        TaskListLabel.NOT_COMPLETED,
        ScapFormTaskListSectionService.PROJECT_PERFORMANCE_DISPLAY_NAME,
        ReverseRouter.route(on(ProjectPerformanceController.class).renderProjectPerformanceForm(SCAP_ID))
    );
  }

  @Test
  void getProjectPerformanceTaskListItem_ValidProjectPerformance() {
    var projectPerformance = new ProjectPerformance();
    var form = new ProjectPerformanceForm();
    var bindingResult = ValidatorTestingUtil.bindingResultWithoutErrors(form);

    when(projectPerformanceService.findByScapDetail(SCAP_DETAIL)).thenReturn(Optional.of(projectPerformance));
    when(projectPerformanceFormService.getForm(projectPerformance)).thenReturn(form);
    when(projectPerformanceFormService.validate(eq(form), any(BindingResult.class))).thenReturn(bindingResult);

    var label = scapFormTaskListSectionService.getProjectPerformanceTaskListItem(SCAP_ID, SCAP_DETAIL);

    assertThat(label).extracting(
        TaskListItem::label,
        TaskListItem::displayName,
        TaskListItem::actionUrl
    ).containsExactly(
        TaskListLabel.COMPLETED,
        ScapFormTaskListSectionService.PROJECT_PERFORMANCE_DISPLAY_NAME,
        ReverseRouter.route(on(ProjectPerformanceController.class).renderProjectPerformanceForm(SCAP_ID))
    );
  }
}
