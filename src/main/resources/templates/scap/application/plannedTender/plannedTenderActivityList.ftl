<#include '../../layout/layout.ftl'>
<#import 'plannedTenderActivity.ftl' as plannedTenderActivity>

<#assign pageTitle = "Planned tender activities" />

<@defaultPage
  htmlTitle=pageTitle
  pageHeading=pageTitle
  pageSize=PageSize.TWO_THIRDS_COLUMN
  errorItems=errorItems
  backLinkUrl=springUrl(backLinkUrl)>

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
    <@plannedTenderActivity.plannedTenderActivityRows plannedTenderDetail=listItem.detail()/>

  </@fdsSummaryList.summaryListCard>
</#macro>
