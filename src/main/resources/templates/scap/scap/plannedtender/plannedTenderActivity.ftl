<#include '../../layout/layout.ftl'>

<#macro plannedTenderActivityList listItems>
  <#list listItems as listItem>
    <@plannedTenderListItem listItem=listItem listId=listItem?index/>
  </#list>
</#macro>

<#macro plannedTenderListItem listItem listId>
  <#assign cardActionsContent>
    <@fdsSummaryList.summaryListCardActionList>
      <@fdsSummaryList.summaryListCardActionItem
        itemUrl=listItem.changeLinkUrl()
        itemText="Change"
        itemScreenReaderText="Planned tender activity ${listId+1}"
      />
      <@fdsSummaryList.summaryListCardActionItem
        itemUrl=springUrl(listItem.deleteLinkUrl())
        itemText="Delete"
        itemScreenReaderText="Planned tender activity ${listId+1}"
      />
    </@fdsSummaryList.summaryListCardActionList>
  </#assign>

  <@fdsSummaryList.summaryListCard
    summaryListId=listId
    headingText="Planned tender activity ${listId+1}"
    cardActionsContent=cardActionsContent
  >
    <@plannedTenderActivityRows plannedTenderActivity=listItem/>
  </@fdsSummaryList.summaryListCard>
</#macro>


<#macro plannedTenderActivityRows plannedTenderActivity>
<#-- @ftlvariable name="plannedTenderActivity" type="uk.co.nstauthority.scap.scap.plannedtender.list.PlannedTenderActivityListItem" -->
  <@fdsSummaryList.summaryListRowNoAction keyText="Scope description">
    ${plannedTenderActivity.detail().scopeDescription!""}
  </@fdsSummaryList.summaryListRowNoAction>
  <@fdsSummaryList.summaryListRowNoAction keyText="Estimated value">
    <#if plannedTenderActivity.detail().estimatedValue?has_content>
      £${plannedTenderActivity.detail().estimatedValue} million
    </#if>
  </@fdsSummaryList.summaryListRowNoAction>
  <@fdsSummaryList.summaryListRowNoAction keyText="Remuneration model">
    <#if plannedTenderActivity.detail().remunerationModel?has_content>
      ${plannedTenderActivity.detail().remunerationModel.displayName!""}
    </#if>
  </@fdsSummaryList.summaryListRowNoAction>
  <@fdsSummaryList.summaryListRowNoAction keyText="Award rationale">
    ${plannedTenderActivity.detail().awardRationale!""}
  </@fdsSummaryList.summaryListRowNoAction>
  <@fdsSummaryList.summaryListRowNoAction keyText="Indicative actual tender start date">
    ${plannedTenderActivity.indicativeActualTenderStartDate()!""}
  </@fdsSummaryList.summaryListRowNoAction>
  <@fdsSummaryList.summaryListRowNoAction keyText="Indicative contract award date">
    ${plannedTenderActivity.indicativeContractAwardDate()!""}
  </@fdsSummaryList.summaryListRowNoAction>
</#macro>
