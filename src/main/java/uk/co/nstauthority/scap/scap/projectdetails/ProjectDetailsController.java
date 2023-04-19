package uk.co.nstauthority.scap.scap.projectdetails;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;
import uk.co.nstauthority.scap.controllerhelper.ControllerHelperService;
import uk.co.nstauthority.scap.endpointvalidation.annotations.ScapHasStatus;
import uk.co.nstauthority.scap.enumutil.YesNo;
import uk.co.nstauthority.scap.fds.addtolist.AddToListItem;
import uk.co.nstauthority.scap.file.UploadedFileView;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.permissionmanagement.endpointsecurity.PermissionsRequiredForScap;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.projectdetails.supportingdocuments.SupportingDocumentService;
import uk.co.nstauthority.scap.scap.projectdetails.supportingdocuments.SupportingDocumentType;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.scap.ScapService;
import uk.co.nstauthority.scap.scap.tasklist.TaskListController;

@Controller
@RequestMapping("{scapId}/project-details")
@PermissionsRequiredForScap(permissions = RolePermission.SUBMIT_SCAP)
@ScapHasStatus(permittedStatuses = ScapDetailStatus.DRAFT)
public class ProjectDetailsController {

  private final ProjectDetailsFormService projectDetailsFormService;
  private final ControllerHelperService controllerHelperService;
  private final ScapDetailService scapDetailService;
  private final ProjectDetailsService projectDetailsService;
  private final SupportingDocumentService supportingDocumentService;

  @Autowired
  ProjectDetailsController(ProjectDetailsFormService projectDetailsFormService,
                           ControllerHelperService controllerHelperService, ScapService scapService,
                           ScapDetailService scapDetailService, ProjectDetailsService projectDetailsService,
                           SupportingDocumentService supportingDocumentService) {
    this.projectDetailsFormService = projectDetailsFormService;
    this.controllerHelperService = controllerHelperService;
    this.scapDetailService = scapDetailService;
    this.projectDetailsService = projectDetailsService;
    this.supportingDocumentService = supportingDocumentService;
  }

  @GetMapping
  public ModelAndView renderProjectDetailsForm(@PathVariable("scapId") ScapId scapId) {
    var scapDetail = scapDetailService.getLatestByScapIdOrThrow(scapId);
    var projectDetails = projectDetailsService.findByScapDetail(scapDetail);
    var projectFacilities = projectDetails
        .map(projectDetailsService::getProjectFacilities)
        .orElse(Collections.emptyList());
    var projectFields = projectDetails
        .map(projectDetailsService::getProjectFields)
        .orElse(Collections.emptyList());

    var projectFacilityIds = projectFacilities
        .stream()
        .map(ProjectFacility::getFacilityId)
        .collect(Collectors.toSet());
    var projectFieldIds = projectFields
        .stream()
        .map(ProjectField::getFieldId)
        .collect(Collectors.toSet());
    var form = projectDetails
        .map(existingProjectDetails -> projectDetailsFormService
            .getForm(existingProjectDetails, projectFacilityIds, projectFieldIds))
        .orElse(new ProjectDetailsForm());
    var preselectedFields = projectDetailsFormService.getPreselectedFields(projectFieldIds);
    var preselectedFacilities = projectDetailsFormService.getPreselectedFacilities(projectFacilityIds);
    var supportingDocuments = projectDetailsFormService.getSupportingDocuments(form);

    return projectDetailsFormModelAndView(scapId, preselectedFields, preselectedFacilities, supportingDocuments)
        .addObject("form", form);
  }

  @PostMapping
  ModelAndView saveProjectDetailsForm(@PathVariable("scapId") ScapId scapId,
                                      @ModelAttribute("form") ProjectDetailsForm form,
                                      BindingResult bindingResult) {
    var scapDetail = scapDetailService.getLatestByScapIdOrThrow(scapId);
    var preselectedFields = projectDetailsFormService.getPreselectedFields(form.getFieldIds());
    var preselectedFacilities = projectDetailsFormService.getPreselectedFacilities(form.getInstallationIds());
    var supportingDocuments = projectDetailsFormService.getSupportingDocuments(form);

    bindingResult = projectDetailsFormService.validate(form, bindingResult);
    return controllerHelperService.checkErrorsAndRedirect(
        bindingResult,
        projectDetailsFormModelAndView(scapId, preselectedFields, preselectedFacilities, supportingDocuments),
        form,
        () -> {
          projectDetailsService.saveProjectDetails(scapDetail, form);
          return ReverseRouter.redirect(on(TaskListController.class).renderTaskList(scapId));
        });
  }

  private ModelAndView projectDetailsFormModelAndView(ScapId scapId,
                                                      List<AddToListItem> preselectedFields,
                                                      List<AddToListItem> preselectedFacilities,
                                                      List<UploadedFileView> supportingDocuments) {
    return new ModelAndView("scap/scap/projectDetails")
        .addObject("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(scapId)))
        .addObject("projectTypesMap", ProjectType.getCheckboxItems())
        .addObject("preselectedFields", preselectedFields)
        .addObject("preselectedFacilities", preselectedFacilities)
        .addObject("hasInstallationsMap", YesNo.getRadioOptions())
        .addObject("fieldSearchRestUrl",
            ReverseRouter.route(on(ProjectDetailsRestController.class)
                .getFieldSearchResults(null)))
        .addObject("facilitiesSearchRestUrl",
            ReverseRouter.route(on(ProjectDetailsRestController.class)
                .getFacilitySearchResults(null)))
        .addObject("supportingDocumentsUploads", supportingDocuments)
        .addObject("supportingDocumentsTemplate",
            supportingDocumentService.buildFileUploadTemplate(
                scapId, SupportingDocumentType.ADDITIONAL_DOCUMENT
            ));
  }
}
