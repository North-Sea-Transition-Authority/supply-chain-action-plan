package uk.co.nstauthority.scap.scap.summary;

import static java.util.Collections.emptyList;
import static java.util.Collections.emptySet;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.List;
import java.util.Set;
import org.springframework.web.servlet.ModelAndView;
import uk.co.fivium.energyportalapi.generated.types.OrganisationGroup;
import uk.co.nstauthority.scap.error.exception.IllegalUtilClassInstantiationException;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventAction;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventSubject;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventView;
import uk.co.nstauthority.scap.scap.casemanagement.consultationrequest.ConsultationRequestController;
import uk.co.nstauthority.scap.scap.casemanagement.consultationrequest.ConsultationRequestForm;
import uk.co.nstauthority.scap.scap.casemanagement.furtherinfo.FurtherInfoController;
import uk.co.nstauthority.scap.scap.casemanagement.furtherinfo.FurtherInfoRequestForm;
import uk.co.nstauthority.scap.scap.casemanagement.qacomments.QaCommentController;
import uk.co.nstauthority.scap.scap.casemanagement.qacomments.QaCommentForm;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.workarea.WorkAreaController;

public class ScapSummaryModelAndViewGenerator {

  private ScapSummaryModelAndViewGenerator() {
    throw new IllegalUtilClassInstantiationException(ScapSummaryModelAndViewGenerator.class);
  }

  public static Generator generator(ScapDetail scapDetail,
                                    ScapSummaryView scapSummaryView) {
    return new Generator(scapDetail, scapSummaryView);
  }

  public static class Generator {
    private final ScapDetail scapDetail;
    private final ScapSummaryView scapSummary;
    private Set<CaseEventSubject> applicableActions = emptySet();
    private OrganisationGroup orgGroup;
    private ScapSubmissionStage scapStatus = ScapSubmissionStage.DRAFT;
    private List<CaseEventView> caseEventTimeline = emptyList();
    private FurtherInfoRequestForm furtherInfoRequestForm = new FurtherInfoRequestForm();
    private QaCommentForm qaCommentForm = new QaCommentForm();
    private ConsultationRequestForm consultationRequestForm = new ConsultationRequestForm();

    public Generator(ScapDetail scapDetail,
                     ScapSummaryView scapSummary) {
      this.scapDetail = scapDetail;
      this.scapSummary = scapSummary;
    }

    public Generator withOrgGroup(OrganisationGroup organisationGroup) {
      this.orgGroup = organisationGroup;
      return this;
    }

    public Generator withScapStatus(ScapSubmissionStage status) {
      this.scapStatus = status;
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
          .addObject("applicableActions", applicableActions);


      addCaseEventTimeline(modelAndView);
      addQaCommentForm(modelAndView);
      addInfoRequestForm(modelAndView);
      addConsultationRequestForm(modelAndView);

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
  }
}
