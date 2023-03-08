<#include '../../layout/layout.ftl'>
<#import '../summary/scapSummary.ftl' as scapSummary>
<#import '../summary/scapSummaryCard.ftl' as scapSummaryCard>
<#import '../timeline/scapTimelineEvents.ftl' as timeline>

<#assign pageTitle = projectName!'' />
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
    <@fdsActionDropdown.actionDropdown dropdownButtonText="Versions">
      <#list availableVersions as value, key>
        <@fdsActionDropdown.actionDropdownItem actionText=key linkAction=true linkActionUrl=springUrl(versionSubmitUrl + value)/>
      </#list>
    </@fdsActionDropdown.actionDropdown>
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

<@defaultPage
htmlTitle=pageTitle
caption=operator
pageHeading=projectReference
pageSize=PageSize.FULL_WIDTH
backLinkUrl=springUrl(backLinkUrl)
>
  <@scapSummaryCard.summaryCard/>
    <@fdsAction.buttonGroup>
      <@fdsForm.htmlForm springUrl(updateScapUrl)>
        <#if applicableActions["UPDATE_SCAP"]??>
          <#if updateInProgress>
            <@fdsAction.button buttonText="Resume SCAP update"/>
            <@fdsAction.link linkText="Delete draft update" linkUrl=springUrl(deleteScapUrl) linkClass="govuk-button govuk-button--secondary" role=true />
          <#else>
            <@fdsAction.button buttonText="Update SCAP"/>
          </#if>
        </#if>
      </@fdsForm.htmlForm>
      <@scapVersions availableVersions/>
    </@fdsAction.buttonGroup>
  <@fdsAction.buttonGroup>
    <#list applicableActions as group, actions>
      <@buttongroup group=group actions=actions/>
    </#list>
  </@fdsAction.buttonGroup>

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
  <#include 'caseActions/consultationResponseAction.ftl'/>
  <#include 'caseActions/infoResponseAction.ftl'/>
  <#include 'caseActions/updateRequestAction.ftl'/>
  <#include 'caseActions/approveScap.ftl'/>
  <#include 'caseActions/withdrawScap.ftl'/>
  <#include 'caseActions/reinstateScap.ftl'/>
</@defaultPage>

