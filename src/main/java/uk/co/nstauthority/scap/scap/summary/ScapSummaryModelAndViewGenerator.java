package uk.co.nstauthority.scap.scap.summary;

import static java.util.Collections.emptyList;
import static java.util.Collections.emptySet;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;
import static uk.co.nstauthority.scap.scap.projectdetails.supportingdocuments.SupportingDocumentType.APPROVAL_DOCUMENT;
import static uk.co.nstauthority.scap.scap.projectdetails.supportingdocuments.SupportingDocumentType.CONSULTATION_REPORT;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import org.springframework.web.servlet.ModelAndView;
import uk.co.fivium.energyportalapi.generated.types.OrganisationGroup;
import uk.co.nstauthority.scap.error.exception.IllegalUtilClassInstantiationException;
import uk.co.nstauthority.scap.file.FileUploadForm;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventAction;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventSubject;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventView;
import uk.co.nstauthority.scap.scap.casemanagement.approval.ScapApprovalController;
import uk.co.nstauthority.scap.scap.casemanagement.approval.ScapApprovalForm;
import uk.co.nstauthority.scap.scap.casemanagement.consultationrequest.ConsultationRequestController;
import uk.co.nstauthority.scap.scap.casemanagement.consultationrequest.ConsultationRequestForm;
import uk.co.nstauthority.scap.scap.casemanagement.consultationresponse.ConsultationResponseController;
import uk.co.nstauthority.scap.scap.casemanagement.consultationresponse.ConsultationResponseForm;
import uk.co.nstauthority.scap.scap.casemanagement.furtherinfo.FurtherInfoController;
import uk.co.nstauthority.scap.scap.casemanagement.furtherinfo.FurtherInfoRequestForm;
import uk.co.nstauthority.scap.scap.casemanagement.furtherinforesponse.FurtherInfoResponseController;
import uk.co.nstauthority.scap.scap.casemanagement.furtherinforesponse.FurtherInfoResponseForm;
import uk.co.nstauthority.scap.scap.casemanagement.qacomments.QaCommentController;
import uk.co.nstauthority.scap.scap.casemanagement.qacomments.QaCommentForm;
import uk.co.nstauthority.scap.scap.casemanagement.update.ScapUpdateController;
import uk.co.nstauthority.scap.scap.casemanagement.withdraw.ScapWithdrawController;
import uk.co.nstauthority.scap.scap.casemanagement.withdraw.ScapWithdrawalForm;
import uk.co.nstauthority.scap.scap.delete.ScapDeletionController;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.projectdetails.supportingdocuments.SupportingDocumentService;
import uk.co.nstauthority.scap.workarea.WorkAreaController;

public class ScapSummaryModelAndViewGenerator {

  private ScapSummaryModelAndViewGenerator() {
    throw new IllegalUtilClassInstantiationException(ScapSummaryModelAndViewGenerator.class);
  }

  public static Generator generator(ScapDetail scapDetail,
                                    ScapSummaryView scapSummaryView,
                                    SupportingDocumentService supportingDocumentService) {
    return new Generator(scapDetail, scapSummaryView, supportingDocumentService);
  }

  public static class Generator {
    private final ScapDetail scapDetail;
    private final ScapSummaryView scapSummary;
    private Set<CaseEventSubject> applicableActions = emptySet();
    private OrganisationGroup orgGroup;
    private boolean updateInProgress = false;
    private final SupportingDocumentService supportingDocumentService;
    private ScapSubmissionStage scapStatus = ScapSubmissionStage.DRAFT;
    private List<CaseEventView> caseEventTimeline = emptyList();
    private FurtherInfoRequestForm furtherInfoRequestForm = new FurtherInfoRequestForm();
    private QaCommentForm qaCommentForm = new QaCommentForm();
    private ConsultationRequestForm consultationRequestForm = new ConsultationRequestForm();
    private FurtherInfoResponseForm furtherInfoResponseForm = new FurtherInfoResponseForm();
    private ScapApprovalForm scapApprovalForm = new ScapApprovalForm();
    private ConsultationResponseForm consultationResponseForm = new ConsultationResponseForm();
    private ScapWithdrawalForm scapWithdrawalForm = new ScapWithdrawalForm();

    private List<FileUploadForm> existingApprovalFiles = new ArrayList<>();

    public Generator(ScapDetail scapDetail,
                     ScapSummaryView scapSummary,
                     SupportingDocumentService supportingDocumentService) {
      this.scapDetail = scapDetail;
      this.scapSummary = scapSummary;
      this.supportingDocumentService = supportingDocumentService;
    }

    public Generator withOrgGroup(OrganisationGroup organisationGroup) {
      this.orgGroup = organisationGroup;
      return this;
    }

    public Generator withScapStatus(ScapSubmissionStage status) {
      this.scapStatus = status;
      return this;
    }

    public Generator withUpdateInProgress(boolean updateInProgress) {
      this.updateInProgress = updateInProgress;
      return this;
    }

    public Generator withCaseEventTimeline(List<CaseEventView> caseEventTimeline) {
      this.caseEventTimeline = caseEventTimeline;
      return this;
    }

    public Generator withFurtherInfoRequestFrom(FurtherInfoRequestForm furtherInfoRequestForm) {
      this.furtherInfoRequestForm = furtherInfoRequestForm;
      return this;
    }

    public Generator withQaCommentForm(QaCommentForm qaCommentForm) {
      this.qaCommentForm = qaCommentForm;
      return this;
    }

    public Generator withConsultationRequestForm(ConsultationRequestForm consultationRequestForm) {
      this.consultationRequestForm = consultationRequestForm;
      return this;
    }

    public Generator withConsultationResponseForm(ConsultationResponseForm consultationResponseForm) {
      this.consultationResponseForm = consultationResponseForm;
      return this;
    }

    public Generator withFurtherInfoResponseForm(FurtherInfoResponseForm furtherInfoResponseForm) {
      this.furtherInfoResponseForm = furtherInfoResponseForm;
      return this;
    }

    public Generator withScapApprovalForm(ScapApprovalForm scapApprovalForm) {
      this.scapApprovalForm = scapApprovalForm;
      return this;
    }

    public Generator withScapApprovalDocuments(List<FileUploadForm> existingApprovalFiles) {
      this.existingApprovalFiles = existingApprovalFiles;
      return this;
    }

    public Generator withScapWithdrawalForm(ScapWithdrawalForm scapWithdrawalForm) {
      this.scapWithdrawalForm = scapWithdrawalForm;
      return this;
    }

    public Generator withApplicableActions(Set<CaseEventSubject> applicableActions) {
      this.applicableActions = applicableActions;
      return this;
    }

    public ModelAndView generate() {
      var modelAndView =  new ModelAndView("scap/scap/summary/scapSummaryOverview")
          .addObject("scapSummaryView", scapSummary)
          .addObject("projectReference", scapDetail.getScap().getReference())
          .addObject("projectName", scapSummary.projectDetailsSummaryView().projectName())
          .addObject("operator", orgGroup != null ? orgGroup.getName() : "")
          .addObject("scapStatus", scapDetail.getStatus().getDisplayName())
          .addObject("scapSubmissionStatus", scapStatus.getDisplayName())
          .addObject("backLinkUrl", ReverseRouter.route(on(WorkAreaController.class).getWorkArea(null)))
          .addObject("updateScapUrl", ReverseRouter.route(
              on(ScapUpdateController.class).startScapUpdate(scapDetail.getScap().getScapId(), CaseEventAction.UPDATE)))
          .addObject("applicableActions", applicableActions)
          .addObject("updateInProgress", updateInProgress)
          .addObject("deleteScapUrl", ReverseRouter.route(
              on(ScapDeletionController.class).renderScapDeletionConfirmation(scapDetail.getScap().getScapId())));


      addCaseEventTimeline(modelAndView);
      addQaCommentForm(modelAndView);
      addInfoRequestForm(modelAndView);
      addInfoResponseForm(modelAndView);
      addConsultationRequestForm(modelAndView);
      addConsultationResponseForm(modelAndView);
      addScapApprovalRequestForm(modelAndView);
      addWithdrawForm(modelAndView);

      return modelAndView;
    }

    private void addCaseEventTimeline(ModelAndView modelAndView) {
      modelAndView.addObject("caseEvents", caseEventTimeline);
    }

    private void addQaCommentForm(ModelAndView modelAndView) {
      modelAndView.addObject("qaForm", qaCommentForm);
      modelAndView.addObject("qaFormSubmitUrl",
          ReverseRouter.route(on(QaCommentController.class)
              .saveQaCommentForm(scapDetail.getScap().getScapId(),
                  CaseEventAction.QA,
                  true,
                  null,
                  null)));
    }

    private void addInfoRequestForm(ModelAndView modelAndView) {
      modelAndView.addObject("infoRequestForm", furtherInfoRequestForm);
      modelAndView.addObject("infoRequestSubmitUrl",
          ReverseRouter.route(on(FurtherInfoController.class)
              .saveInfoRequestedForm(scapDetail.getScap().getScapId(),
                  CaseEventAction.INFO_REQUESTED,
                  true,
                  null,
                  null)));
    }

    private void addInfoResponseForm(ModelAndView modelAndView) {
      modelAndView.addObject("infoResponseForm", furtherInfoResponseForm);
      modelAndView.addObject("infoResponseSubmitUrl",
          ReverseRouter.route(on(FurtherInfoResponseController.class)
              .saveInfoResponseForm(scapDetail.getScap().getScapId(),
                  CaseEventAction.INFO_RESPONSE,
                  true,
                  null,
                  null)));
    }

    private void addConsultationRequestForm(ModelAndView modelAndView) {
      modelAndView.addObject("consultationRequestForm", consultationRequestForm);
      modelAndView.addObject("consultationRequestSubmitUrl",
          ReverseRouter.route(on(ConsultationRequestController.class)
              .saveConsultationRequestForm(scapDetail.getScap().getScapId(),
                  CaseEventAction.CONSULTATION_REQUESTED,
                  true,
                  null,
                  null)));
    }

    private void addConsultationResponseForm(ModelAndView modelAndView) {
      modelAndView.addObject("consultationResponseForm", consultationResponseForm);
      modelAndView.addObject("consultationResponseSubmitUrl",
          ReverseRouter.route(on(ConsultationResponseController.class)
              .saveConsultationResponseForm(scapDetail.getScap().getScapId(),
                  CaseEventAction.CONSULTATION_RESPONSE,
                  true,
                  null,
                  null)));
      modelAndView.addObject("supportingDocumentsTemplate",
          supportingDocumentService.buildFileUploadTemplate(scapDetail.getScap().getScapId(), CONSULTATION_REPORT));
    }

    private void addScapApprovalRequestForm(ModelAndView modelAndView) {
      modelAndView.addObject("scapApprovalForm", scapApprovalForm);
      modelAndView.addObject("projectClosedOut", scapSummary.projectPerformanceSummaryView().isProjectCompleted());
      modelAndView.addObject("approvalDocumentUploads", existingApprovalFiles);
      modelAndView.addObject("approvalFormSubmitUrl",
          ReverseRouter.route(on(ScapApprovalController.class)
              .saveScapApprovalForm(scapDetail.getScap().getScapId(),
                  CaseEventAction.APPROVED,
                  true,
                  null,
                  null)));
      modelAndView.addObject("approvalDocumentsTemplate",
          supportingDocumentService.buildFileUploadTemplate(scapDetail.getScap().getScapId(), APPROVAL_DOCUMENT));
    }

    private void addWithdrawForm(ModelAndView modelAndView) {
      modelAndView.addObject("scapWithdrawForm", scapWithdrawalForm);
      modelAndView.addObject("WithdrawScapUrl",
          ReverseRouter.route(on(ScapWithdrawController.class).withdrawScap(scapDetail.getScap().getScapId(),
              CaseEventAction.WITHDRAWN,
              true,
              null,
              null,
              null)));
    }
  }
}
