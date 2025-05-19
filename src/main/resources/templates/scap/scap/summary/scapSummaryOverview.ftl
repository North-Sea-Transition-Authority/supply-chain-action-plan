<#include '../../layout/layout.ftl'>
<#import '../summary/scapSummary.ftl' as scapSummary>
<#import '../summary/scapSummaryCard.ftl' as scapSummaryCard>
<#import '../timeline/scapTimelineEvents.ftl' as timeline>
<#import '../../scap/macros/noObjectionGuidance.ftl' as noObjectionGuidance>

<#assign pageTitle = projectName!'' />
<#assign customerMnemonic = customerBranding.mnemonic() />

<#macro buttongroup group actions>
    <#if actions?size gt 1>
      <@fdsActionDropdown.actionDropdown dropdownButtonText="${group}">
        <#list actions as action>
          <@fdsActionDropdown.actionDropdownItem actionText="${action.getButtonText()}" buttonSlideOutPanelId="${action.getActionPanelId()}"/>
        </#list>
      </@fdsActionDropdown.actionDropdown>
    <#elseif actions?size == 1>
      <#list actions as action>
        <@fdsAction.button buttonText="${action.getButtonText()}" buttonSlideOutPanelId="${action.getActionPanelId()}"buttonClass="govuk-button govuk-button--secondary"/>
      </#list>
    </#if>

</#macro>

<#macro scapVersions availableVersions>
  <@fdsForm.htmlForm actionUrl=springUrl(versionSubmitUrl)>
    <div class="inline-input-action">

      <@fdsSelect.select
        path="versionSelectForm.requestedVersion"
        options=availableVersions
        labelText="Change the selected version"
      />
      <@fdsAction.button
        buttonText="View"
        buttonClass="govuk-button govuk-button--secondary"
      />
    </div>
  </@fdsForm.htmlForm>
</#macro>

<#-- @ftlvariable name="backLinkUrl" type="java.lang.String" -->
<#-- @ftlvariable name="updateScapUrl" type="java.lang.String" -->
<#-- @ftlvariable name="projectReference" type="java.lang.String"-->
<#-- @ftlvariable name="projectName" type="java.lang.String"-->
<#-- @ftlvariable name="operator" type="java.lang.String" -->
<#-- @ftlvariable name="scapSummaryView" type="uk.co.nstauthority.scap.scap.summary.ScapSummaryView" -->
<#-- @ftlvariable name="caseEvents" type="java.util.List<uk.co.nstauthority.scap.scap.casemanagement.CaseEventView>" -->
<#-- @ftlvariable name="applicableActions" type="java.util.Map<uk.co.nstauthority.scap.scap.casemanagement.CaseEventGroups, uk.co.nstauthority.scap.scap.casemanagement.CaseEventSubject>" -->
<#-- @ftlvariable name="updateInProgress" type="java.lang.Boolean" -->
<#-- @ftlvariable name="isUpdateable" type="java.lang.Boolean" -->

<@defaultPage
  htmlTitle=pageTitle
  caption=operator
  pageHeading=projectReference
  pageSize=PageSize.FULL_WIDTH
  backLinkUrl=springUrl(backLinkUrl)
>
  <@scapSummaryCard.summaryCard/>
  <#if updateText?has_content>
    <@fdsNotificationBanner.notificationBannerInfo bannerTitleText="An update has been requested for this SCAP">
      <@fdsNotificationBanner.notificationBannerContent headingText=" ">
        Update your SCAP to provide the information requested.
        <@fdsDetails.summaryDetails summaryTitle="What information has the ${customerMnemonic} asked to be updated?">
          <p class="govuk-body">
            ${updateText}
          </p>
        </@fdsDetails.summaryDetails>
      </@fdsNotificationBanner.notificationBannerContent>
    </@fdsNotificationBanner.notificationBannerInfo>
  </#if>
  <@fdsAction.buttonGroup>
    <@fdsForm.htmlForm springUrl(updateScapUrl)>
      <#if isUpdateable>
        <#if updateInProgress>
          <@fdsAction.button buttonText="Resume SCAP update"/>
          <@fdsAction.link linkText="Delete draft update" linkUrl=springUrl(deleteScapUrl) linkClass="govuk-button govuk-button--secondary" role=true />
        <#else>
          <@fdsAction.button buttonText="Update SCAP"/>
        </#if>
      </#if>
    </@fdsForm.htmlForm>
  </@fdsAction.buttonGroup>
  <@fdsAction.buttonGroup>
    <#list applicableActions as group, actions>
      <@buttongroup group=group actions=actions/>
    </#list>
    <@fdsAction.link linkText="Print" linkUrl="javascript:window.print()" linkClass="govuk-button govuk-button--secondary"/>
  </@fdsAction.buttonGroup>
  <@fdsDetails.summaryDetails detailsClass="print-details" summaryTitle="How can I save a PDF copy of the application?">
    <p class="govuk-body">
      Click the link to print your application. In the print dialog displayed change your printer to ‘Print to PDF’. This will allow you to save the application as a PDF.</p>
  </@fdsDetails.summaryDetails>

  <@noObjectionGuidance.noObjectionGuidance/>

  <@fdsTabs.tabs tabsHeading="SCAP overview tabs">
    <@fdsTabs.tabList>
      <@fdsTabs.tab tabLabel="Application form" tabAnchor="summary-tab"/>
      <#if caseEvents?has_content>
        <@fdsTabs.tab tabLabel="Case events" tabAnchor="events-tab"/>
      </#if>
    </@fdsTabs.tabList>
    <@fdsTabs.tabContent tabAnchor="summary-tab">
      <#if currentVersionTitle?has_content>
        <h2 class="govuk-heading-l">${currentVersionTitle}</h2>
      </#if>
      <@scapVersions availableVersions/>
      <@scapSummary.summary scapSummaryView/>
    </@fdsTabs.tabContent>
      <#if caseEvents?has_content>
        <@fdsTabs.tabContent tabAnchor="events-tab">
          <h2 class="govuk-heading-l">Case event timeline</h2>
          <@timeline.timeline caseEvents=caseEvents/>
        </@fdsTabs.tabContent>
      </#if>
  </@fdsTabs.tabs>
  <#include 'caseActions/qaCommentAction.ftl'/>
  <#include 'caseActions/infoRequestAction.ftl'/>
  <#include 'caseActions/consultationRequestAction.ftl'/>
  <#include 'caseActions/consultationResponseAction.ftl'/>
  <#include 'caseActions/infoResponseAction.ftl'/>
  <#include 'caseActions/updateRequestAction.ftl'/>
  <#include 'caseActions/approveScap.ftl'/>
  <#include 'caseActions/withdrawScap.ftl'/>
  <#include 'caseActions/reinstateScap.ftl'/>
</@defaultPage>
