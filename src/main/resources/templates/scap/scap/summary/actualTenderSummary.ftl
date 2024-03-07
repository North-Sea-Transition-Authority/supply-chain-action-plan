<#include '../../layout/layout.ftl'>
<#import '../actualtender/actualTenderActivitySummaryContent.ftl' as actualTenderSummaryContent>

<#assign actualTenderMissingData = "No information on actual tender activities has been provided">

<#macro actualTenderSummary actualTenderSummaryView>
<#-- @ftlvariable name="actualTenderSummaryView" type="uk.co.nstauthority.scap.scap.summary.actualtender.ActualTenderSummaryView" -->
  <#if actualTenderSummaryView.hasActualTenderActivities()?has_content>

    <#if actualTenderSummaryView.actualTenderActivitySummaryViews()?has_content >
      <#list actualTenderSummaryView.actualTenderActivitySummaryViews() as actualTenderActivitySummaryView>
        <@fdsSummaryList.summaryListCard
          summaryListId="actual-tender-summary-${actualTenderActivitySummaryView_index}"
          headingText="Actual tender activity ${actualTenderActivitySummaryView_index + 1}"
        >
          <@actualTenderSummaryContent.actualTenderActivitySummaryContent
            actualTenderActivity=actualTenderActivitySummaryView
          />
        </@fdsSummaryList.summaryListCard>
      </#list>

    <#elseif actualTenderSummaryView.hasActualTenderActivities() >
      <@fdsInsetText.insetText>
        ${actualTenderMissingData}
      </@fdsInsetText.insetText>

    <#else>
      <@fdsInsetText.insetText>
        This SCAP does not have any actual tender activities
      </@fdsInsetText.insetText>
    </#if>

  <#else>
    <@fdsInsetText.insetText>
      ${actualTenderMissingData}
    </@fdsInsetText.insetText>
  </#if>
</#macro>
