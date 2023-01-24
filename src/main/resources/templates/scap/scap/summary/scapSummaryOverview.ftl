<#include '../../layout/layout.ftl'>
<#import '../summary/scapSummary.ftl' as scapSummary>
<#import '../summary/scapSummaryCard.ftl' as scapSummaryCard>
<#import '../timeline/scapTimelineEvents.ftl' as timeline>

<#assign pageTitle = projectName!'' />

<#-- @ftlvariable name="backLinkUrl" type="java.lang.String" -->
<#-- @ftlvariable name="projectReference" type="java.lang.String"-->
<#-- @ftlvariable name="projectName" type="java.lang.String"-->
<#-- @ftlvariable name="operator" type="java.lang.String" -->
<#-- @ftlvariable name="scapSummaryView" type="uk.co.nstauthority.scap.scap.summary.ScapSummaryView" -->
<#-- @ftlvariable name="caseEvents" type="java.util.List<uk.co.nstauthority.scap.scap.casemanagement.CaseEventView>" -->

<@defaultPage
htmlTitle=pageTitle
caption=operator
pageHeading=projectReference
pageSize=PageSize.FULL_WIDTH
backLinkUrl=springUrl(backLinkUrl)
>
  <@scapSummaryCard.summaryCard/>
  <@fdsSlideOutPanel.slideOutPanelButton buttonText="Complete QA checks" buttonPanelId="Qa-Panel" buttonClass="govuk-button govuk-button--secondary"/>
  <@fdsSlideOutPanel.slideOutPanelButton buttonText="Request further information" buttonPanelId="Info-Request-Panel" buttonClass="govuk-button govuk-button--secondary"/>
  <@fdsSlideOutPanel.slideOutPanelButton buttonText="Request Consultation" buttonPanelId="Consultation-Request-Panel" buttonClass="govuk-button govuk-button--secondary"/>

  <@fdsTabs.tabs tabsHeading="SCAP overview tabs">
    <@fdsTabs.tabList>
      <@fdsTabs.tab tabLabel="Application form" tabAnchor="summary-tab"/>
      <#if caseEvents?has_content>
        <@fdsTabs.tab tabLabel="Case events" tabAnchor="events-tab"/>
      </#if>
    </@fdsTabs.tabList>
    <@fdsTabs.tabContent tabAnchor="summary-tab">
      <@scapSummary.summary scapSummaryView/>
    </@fdsTabs.tabContent>
      <#if caseEvents?has_content>
        <@fdsTabs.tabContent tabAnchor="events-tab">
          <@timeline.timeline caseEvents=caseEvents/>
        </@fdsTabs.tabContent>
      </#if>
  </@fdsTabs.tabs>
  <#include 'caseActions/qaCommentAction.ftl'/>
  <#include 'caseActions/infoRequestAction.ftl'/>
  <#include 'caseActions/consultationRequestAction.ftl'/>
</@defaultPage>
