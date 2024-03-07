<#include '../../layout/layout.ftl'>

<#assign plannedTenderMissingData = "No information on planned tender activities has been provided">

<#macro plannedTenderSummary plannedTenderSummaryView>
<#-- @ftlvariable name="plannedTenderSummaryView" type="uk.co.nstauthority.scap.scap.summary.plannedtender.PlannedTenderSummaryView" -->
  <#if plannedTenderSummaryView.hasPlannedTender()?has_content>

    <#if plannedTenderSummaryView.plannedTenderActivitySummaryViews()?has_content >
      <#list plannedTenderSummaryView.plannedTenderActivitySummaryViews() as plannedTenderActivitySummaryView>
        <@plannedTenderActivitySummary
          plannedTenderActivitySummaryView=plannedTenderActivitySummaryView
          index=plannedTenderActivitySummaryView_index
        />
      </#list>
    <#elseif plannedTenderSummaryView.hasPlannedTender() >
      <@fdsInsetText.insetText>
        ${plannedTenderMissingData}
      </@fdsInsetText.insetText>
    <#else>
      <@fdsInsetText.insetText>
        This SCAP does not have any planned tender activities
      </@fdsInsetText.insetText>
    </#if>

  <#else>
    <@fdsInsetText.insetText>
      ${plannedTenderMissingData}
    </@fdsInsetText.insetText>
  </#if>

</#macro>

<#macro plannedTenderActivitySummary plannedTenderActivitySummaryView index>
  <@fdsSummaryList.summaryListCard
    summaryListId="planned-tender-activity-summary-${index}"
    headingText="Planned tender activity ${index + 1}"
  >
    <@plannedTenderActivitySummaryContent plannedTenderActivitySummaryView=plannedTenderActivitySummaryView />
  </@fdsSummaryList.summaryListCard>

</#macro>

<#macro plannedTenderActivitySummaryContent plannedTenderActivitySummaryView>
<#-- @ftlvariable name="plannedTenderActivitySummaryView" type="uk.co.nstauthority.scap.scap.summary.plannedtender.PlannedTenderActivitySummaryView" -->
  <@fdsSummaryList.summaryListRowNoAction keyText="Scope description">
    ${plannedTenderActivitySummaryView.scopeDescription()!""}
  </@fdsSummaryList.summaryListRowNoAction>

  <@fdsSummaryList.summaryListRowNoAction keyText="Estimated value">
    <#if plannedTenderActivitySummaryView.estimatedValue()?has_content>
      £${plannedTenderActivitySummaryView.estimatedValue()} million
    </#if>
  </@fdsSummaryList.summaryListRowNoAction>

  <@fdsSummaryList.summaryListRowNoAction keyText="Remuneration model">
    <#if plannedTenderActivitySummaryView.remunerationModel()?has_content>
      ${plannedTenderActivitySummaryView.remunerationModel().displayName!""}
    </#if>
  </@fdsSummaryList.summaryListRowNoAction>

  <#if plannedTenderActivitySummaryView.remunerationModelName()?has_content>
    <@fdsSummaryList.summaryListRowNoAction keyText="Provide the remuneration model" >
      ${plannedTenderActivitySummaryView.remunerationModelName()!""}
    </@fdsSummaryList.summaryListRowNoAction>
  </#if>

  <@fdsSummaryList.summaryListRowNoAction keyText="Award rationale" >
    ${plannedTenderActivitySummaryView.awardRationale()!""}
  </@fdsSummaryList.summaryListRowNoAction>

    <@fdsSummaryList.summaryListRowNoAction keyText="Indicative actual tender start date">
        ${plannedTenderActivitySummaryView.indicativeStartDate()!""}
    </@fdsSummaryList.summaryListRowNoAction>

    <@fdsSummaryList.summaryListRowNoAction keyText="Indicative contract award date">
        ${plannedTenderActivitySummaryView.indicativeAwardDate()!""}
    </@fdsSummaryList.summaryListRowNoAction>
</#macro>
