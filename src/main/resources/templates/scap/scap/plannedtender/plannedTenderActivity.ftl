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
        itemScreenReaderText="Change this planned tender activity"
      />
      <@fdsSummaryList.summaryListCardActionItem
        itemUrl=springUrl(listItem.deleteLinkUrl())
        itemText="Delete"
        itemScreenReaderText="Delete this planned tender activity"
      />
    </@fdsSummaryList.summaryListCardActionList>
  </#assign>

  <@fdsSummaryList.summaryListCard
    summaryListId=listId
    headingText="Planned tender activity"
    cardActionsContent=cardActionsContent
  >
    <@plannedTenderActivityRows plannedTenderDetail=listItem.detail()/>
  </@fdsSummaryList.summaryListCard>
</#macro>


<#macro plannedTenderActivityRows plannedTenderDetail>
  <@fdsSummaryList.summaryListRowNoAction keyText="Scope description">
    ${plannedTenderDetail.scopeDescription!"No scope description"}
  </@fdsSummaryList.summaryListRowNoAction>
  <@fdsSummaryList.summaryListRowNoAction keyText="Estimated value">
    <#if plannedTenderDetail.estimatedValue?has_content>
      £${plannedTenderDetail.estimatedValue} million
    <#else>
      No estimated value
    </#if>
  </@fdsSummaryList.summaryListRowNoAction>
  <@fdsSummaryList.summaryListRowNoAction keyText="Remuneration model">
    ${plannedTenderDetail.remunerationModel.displayName!"No remuneration model"}
  </@fdsSummaryList.summaryListRowNoAction>
  <@fdsSummaryList.summaryListRowNoAction keyText="Award rationale">
    ${plannedTenderDetail.awardRationale!"No award rationale"}
  </@fdsSummaryList.summaryListRowNoAction>
</#macro>
