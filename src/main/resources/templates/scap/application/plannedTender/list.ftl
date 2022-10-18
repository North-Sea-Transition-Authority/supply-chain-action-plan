<#include '../../layout/layout.ftl'>

<#assign pageTitle = "Planned tender activity" />

<@defaultPage
htmlTitle=pageTitle
pageHeading=pageTitle
pageSize=PageSize.TWO_THIRDS_COLUMN
backLinkUrl=springUrl(backLinkUrl)
>
  <#list plannedTenderDetailsList as listItem>
    <@plannedTenderListItem listItem=listItem listId=listItem?index/>
  </#list>
</@defaultPage>


<#macro plannedTenderListItem listItem listId>
  <#assign cardActionsContent>
    <@fdsSummaryList.summaryListCardActionList>
      <@fdsSummaryList.summaryListCardActionItem
        itemUrl=listItem.changeLinkUrl()
        itemText="Change"
        itemScreenReaderText="Change this planned tender activity"
      />
      <@fdsSummaryList.summaryListCardActionItem
        itemUrl=listItem.deleteLinkUrl()
        itemText="Remove"
        itemScreenReaderText="Change this planned tender activity"
      />
    </@fdsSummaryList.summaryListCardActionList>
  </#assign>

  <@fdsSummaryList.summaryListCard
    summaryListId=listId
    headingText="Planned tender activity"
      cardActionsContent=cardActionsContent
  >
    <@fdsSummaryList.summaryListRowNoAction keyText="Scope description">
      <#if listItem.detail().scopeDescription?has_content>
        ${listItem.detail().scopeDescription}
      <#else>
        No scope description
      </#if>
    </@fdsSummaryList.summaryListRowNoAction>
    <@fdsSummaryList.summaryListRowNoAction keyText="Estimated value">
      <#if listItem.detail().estimatedValue?has_content>
        £${listItem.detail().estimatedValue} million
      <#else>
        No estimated value
      </#if>
    </@fdsSummaryList.summaryListRowNoAction>
    <@fdsSummaryList.summaryListRowNoAction keyText="Remuneration model">
      <#if listItem.detail().remunerationModel.displayName?has_content>
        ${listItem.detail().remunerationModel.displayName}
      <#else>
        No remuneration model
      </#if>
    </@fdsSummaryList.summaryListRowNoAction>
    <@fdsSummaryList.summaryListRowNoAction keyText="Award rationale">
      <#if listItem.detail().awardRationale?has_content>
        ${listItem.detail().awardRationale}
      <#else>
        No award rationale
      </#if>
    </@fdsSummaryList.summaryListRowNoAction>

  </@fdsSummaryList.summaryListCard>
</#macro>
