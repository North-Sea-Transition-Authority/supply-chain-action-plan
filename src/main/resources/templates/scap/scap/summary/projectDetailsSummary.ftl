<#include '../../layout/layout.ftl'>

<#macro projectDetailsSummary projectDetailsView>
<#-- @ftlvariable name="projectDetailsView" type="uk.co.nstauthority.scap.scap.summary.ProjectDetailsSummaryView" -->
  <@fdsSummaryList.summaryListCard
    headingText="Project details"
    summaryListId="project-details-summary-card"
  >
    <@fdsSummaryList.summaryListRowNoAction keyText="Project name">
      ${projectDetailsView.projectName()!""}
    </@fdsSummaryList.summaryListRowNoAction>
    <@fdsSummaryList.summaryListRowNoAction keyText="Project summary">
        ${projectDetailsView.projectSummary()!""}
    </@fdsSummaryList.summaryListRowNoAction>
    <@fdsSummaryList.summaryListRowNoAction keyText="Project types">
      <ul class="govuk-list">
        <#list projectDetailsView.projectTypes() as projectType>
          <li>${projectType.displayName}</li>
        </#list>
      </ul>
    </@fdsSummaryList.summaryListRowNoAction>
    <@fdsSummaryList.summaryListRowNoAction keyText="Project cost estimate">
      <#if projectDetailsView.projectCostEstimate()?has_content>
        £${projectDetailsView.projectCostEstimate()} million
      </#if>
    </@fdsSummaryList.summaryListRowNoAction>
    <@fdsSummaryList.summaryListRowNoAction keyText="Do you anticipate that this project will meet the target to achieve 50% UK content?">
      <#if projectDetailsView.expectsToMeetLocalContentCommitment()?has_content>
        ${projectDetailsView.expectsToMeetLocalContentCommitment()?then('Yes', 'No')}
      </#if>
    </@fdsSummaryList.summaryListRowNoAction>
    <#if projectDetailsView.whyNotExpectingToMeetLocalContentCommitment()?has_content>
      <@fdsSummaryList.summaryListRowNoAction keyText="Rationale on why you do not expect the target will be reached">
        <#if projectDetailsView.expectsToMeetLocalContentCommitment()?has_content>
          ${projectDetailsView.whyNotExpectingToMeetLocalContentCommitment()}
        </#if>
      </@fdsSummaryList.summaryListRowNoAction>
    </#if>
    <#if projectDetailsView.fieldNames()?has_content>
      <#list projectDetailsView.fieldNames() as field>
        <@fdsSummaryList.summaryListRowNoAction keyText="Field name ${field_index + 1}">
          ${field}
        </@fdsSummaryList.summaryListRowNoAction>
      </#list>
    </#if>
    <@fdsSummaryList.summaryListRowNoAction keyText="Are any installations or subsea infrastructure related to this project?">
      <#if projectDetailsView.hasFacilities()?has_content>
        ${projectDetailsView.hasFacilities().displayName}
      </#if>
    </@fdsSummaryList.summaryListRowNoAction>
    <#if projectDetailsView.hasFacilities()?has_content && projectDetailsView.hasFacilities() == "YES">
      <@fdsSummaryList.summaryListRowNoAction keyText="Related installations and subsea infrastructure">
        <ul class="govuk-list">
          <#list projectDetailsView.projectFacilities() as projectFacility>
            <li>${projectFacility}</li>
          </#list>
        </ul>
      </@fdsSummaryList.summaryListRowNoAction>
    </#if>
    <@fdsSummaryList.summaryListRowNoAction keyText="Planned execution start date">
      ${projectDetailsView.plannedExecutionStartDate()!""}
    </@fdsSummaryList.summaryListRowNoAction>
    <@fdsSummaryList.summaryListRowNoAction keyText="Planned completion date">
      ${projectDetailsView.plannedCompletionDate()!""}
    </@fdsSummaryList.summaryListRowNoAction>
    <@fileUploadSummaryContent projectDetailsView.supportingDocuments() />

  </@fdsSummaryList.summaryListCard>
</#macro>

<#macro fileUploadSummaryContent fileUploadSummaryViews>
<#-- @ftlvariable name="fileUploadSummaryViews" type="java.util.List<uk.co.nstauthority.scap.scap.summary.files.FileUploadSummaryView>" -->
  <#if fileUploadSummaryViews?has_content>
    <#list fileUploadSummaryViews as fileUploadSummaryView>
      <@fdsSummaryList.summaryListRowNoAction keyText="File name">
        <@fdsAction.link linkText=fileUploadSummaryView.fileName()!"" linkUrl=springUrl(fileUploadSummaryView.fileUrl())/>
      </@fdsSummaryList.summaryListRowNoAction>
      <@fdsSummaryList.summaryListRowNoAction keyText="File description">
        ${fileUploadSummaryView.fileDescription()!""}
      </@fdsSummaryList.summaryListRowNoAction>
    </#list>
  <#else>
    <@fdsSummaryList.summaryListRowNoAction keyText="File name" />
    <@fdsSummaryList.summaryListRowNoAction keyText="File description" />
  </#if>
</#macro>
