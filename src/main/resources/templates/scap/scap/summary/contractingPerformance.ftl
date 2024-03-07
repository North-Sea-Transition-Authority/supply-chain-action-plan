<#include '../../layout/layout.ftl'>
<#import '../contractingperformance/contractingPerformanceSummaryContent.ftl' as contractingPerformanceOverviewSummaryContent>

<#assign contractingPerformanceOverviewMissingData = "No information on contracting performance has been provided">

<#macro contractingPerformanceOverviewSummary contractingPerformanceOverviewSummaryView>
<#-- @ftlvariable name="contractingPerformanceOverviewSummaryView" type="uk.co.nstauthority.scap.scap.summary.contractingperformance.ContractingPerformanceOverviewSummaryView" -->
  <#if contractingPerformanceOverviewSummaryView.hasContractingPerformance()?has_content>

    <#if contractingPerformanceOverviewSummaryView.contractingPerformanceSummaryViews()?has_content >
      <#list contractingPerformanceOverviewSummaryView.contractingPerformanceSummaryViews() as contractingPerformanceSummaryView>
        <@fdsSummaryList.summaryListCard
          summaryListId="contracting-performance-summary-${contractingPerformanceSummaryView_index}"
          headingText=contractingPerformanceSummaryView.scopeTitle()!""
        >
          <@contractingPerformanceOverviewSummaryContent.contractingPerformanceSummaryContent
            summaryView=contractingPerformanceSummaryView
          />
        </@fdsSummaryList.summaryListCard>
      </#list>

    <#elseif contractingPerformanceOverviewSummaryView.hasContractingPerformance() >
      <@fdsInsetText.insetText>
          ${contractingPerformanceOverviewMissingData}
      </@fdsInsetText.insetText>

    <#else>
      <@fdsInsetText.insetText>
        This SCAP does not have any contracting performance
      </@fdsInsetText.insetText>
    </#if>

  <#else>
      <@fdsInsetText.insetText>
          ${contractingPerformanceOverviewMissingData}
      </@fdsInsetText.insetText>
  </#if>
</#macro>
