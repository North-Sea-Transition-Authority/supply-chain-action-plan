<#include '../../layout/layout.ftl'>

<#macro projectDetailsSummary projectDetailsView>
<#-- @ftlvariable name="projectDetailsView" type="uk.co.nstauthority.scap.scap.submit.submissionviews.ProjectDetailsSubmissionView" -->
  <@fdsSummaryList.summaryListCard
    headingText="Project details"
    summaryListId="project-details-summary-card"
  >
    <@fdsSummaryList.summaryListRowNoAction keyText="Project name">
      ${projectDetailsView.projectName()!""}
    </@fdsSummaryList.summaryListRowNoAction>
    <@fdsSummaryList.summaryListRowNoAction keyText="Project types">
      <ul class="govuk-list">
        <#list projectDetailsView.projectTypes() as projectType>
          <p>${projectType.displayName}</p>
        </#list>
      </ul>
    </@fdsSummaryList.summaryListRowNoAction>
    <@fdsSummaryList.summaryListRowNoAction keyText="Project cost estimate">
      <#if projectDetailsView.projectCostEstimate()?has_content>
        £${projectDetailsView.projectCostEstimate()} million
      </#if>
    </@fdsSummaryList.summaryListRowNoAction>
    <@fdsSummaryList.summaryListRowNoAction keyText="Estimated value local content">
      <#if projectDetailsView.projectCostEstimate()?has_content>
        £${projectDetailsView.estimatedValueLocalContent()} million
      </#if>
    </@fdsSummaryList.summaryListRowNoAction>
    <@fdsSummaryList.summaryListRowNoAction keyText="Field name">
      ${projectDetailsView.fieldName()!""}
    </@fdsSummaryList.summaryListRowNoAction>
    <@fdsSummaryList.summaryListRowNoAction keyText="Are any installations or subsea infrastructure related to this project?">
      <#if projectDetailsView.hasFacilities()?has_content>
        ${projectDetailsView.hasFacilities().displayName}
      </#if>
    </@fdsSummaryList.summaryListRowNoAction>
    <#if projectDetailsView.hasFacilities()?has_content && projectDetailsView.hasFacilities() == "YES">
      <@fdsSummaryList.summaryListRowNoAction keyText="Related installations and subsea infrastructure">
        <ul class="govuk-list">
          <#list projectDetailsView.projectFacilities() as projectFacility>
            <p>${projectFacility}</p>
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
  </@fdsSummaryList.summaryListCard>
</#macro>
