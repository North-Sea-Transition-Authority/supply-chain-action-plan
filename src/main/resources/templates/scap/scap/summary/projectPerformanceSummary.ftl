<#include '../../layout/layout.ftl'>

<#macro projectPerformanceSummary projectPerformanceSummaryView>
<#-- @ftlvariable name="projectPerformanceSummaryView" type="uk.co.nstauthority.scap.scap.summary.ProjectPerformanceSummaryView" -->
  <#if projectPerformanceSummaryView.isProjectCompleted()?has_content>
    <#if projectPerformanceSummaryView.isProjectCompleted()>
      <@projectPerformanceSummaryCard projectPerformanceSummaryView=projectPerformanceSummaryView />
    <#else>
      <@fdsInsetText.insetText>
        The project associated with this SCAP is not yet closed out
      </@fdsInsetText.insetText>
    </#if>
  <#else>
    <@fdsInsetText.insetText>
      No information on project performance and close-out has been provided
    </@fdsInsetText.insetText>
  </#if>

</#macro>

<#macro projectPerformanceSummaryCard projectPerformanceSummaryView>
<#-- @ftlvariable name="projectPerformanceSummaryView" type="uk.co.nstauthority.scap.scap.summary.ProjectPerformanceSummaryView" -->
  <@fdsSummaryList.summaryListCard
    headingText="Project performance and close-out"
    summaryListId="project-performance-summary-card"
  >
    <@fdsSummaryList.summaryListRowNoAction keyText="Actual execution start date">
      ${projectPerformanceSummaryView.startDate()!""}
    </@fdsSummaryList.summaryListRowNoAction>
    <@fdsSummaryList.summaryListRowNoAction keyText="Actual commissioning or completion date">
      ${projectPerformanceSummaryView.completionDate()!""}
    </@fdsSummaryList.summaryListRowNoAction>
    <@fdsSummaryList.summaryListRowNoAction keyText="Project outturn cost">
      <#if projectPerformanceSummaryView.outturnCost()?has_content>
        £${projectPerformanceSummaryView.outturnCost()} million
      </#if>
    </@fdsSummaryList.summaryListRowNoAction>
  </@fdsSummaryList.summaryListCard>
</#macro>
