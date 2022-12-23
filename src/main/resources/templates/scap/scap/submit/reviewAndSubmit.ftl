<#include '../../layout/layout.ftl'>
<#import '../summary/scapSummary.ftl' as scapSummary>

<#assign pageTitle = "Check your answers before submitting your SCAP" />

<#-- @ftlvariable name="projectDetailsSummaryView" type="uk.co.nstauthority.scap.scap.summary.ProjectDetailsSummaryView" -->
<#-- @ftlvariable name="plannedTenderSummaryView" type="uk.co.nstauthority.scap.scap.summary.plannedtender.PlannedTenderSummaryView" -->

<@defaultPage
  htmlTitle=pageTitle
  pageHeading=pageTitle
  pageSize=PageSize.TWO_THIRDS_COLUMN
  backLinkUrl=springUrl(backLinkUrl)
>
  <@scapSummary.summary
    projectDetailsSummaryView=projectDetailsSummaryView
    plannedTenderSummaryView=plannedTenderSummaryView
  />

  <@fdsForm.htmlForm>
    <@fdsAction.submitButtons
      primaryButtonText="Submit"
      linkSecondaryAction=true
      linkSecondaryActionUrl=springUrl(backLinkUrl)
      secondaryLinkText="Cancel"
    />
  </@fdsForm.htmlForm>
</@defaultPage>
