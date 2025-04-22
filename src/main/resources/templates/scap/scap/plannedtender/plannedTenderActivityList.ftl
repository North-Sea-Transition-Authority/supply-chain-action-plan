<#include '../../layout/layout.ftl'>
<#import 'plannedTenderActivity.ftl' as plannedTenderActivity>

<#assign pageTitle = "Planned tender activities" />

<#-- @ftlvariable name="errorList" type="java.util.List<uk.co.nstauthority.scap.fds.ErrorItem>" -->

<@defaultPage
  htmlTitle=pageTitle
  pageHeading=pageTitle
  pageSize=PageSize.TWO_THIRDS_COLUMN
  errorItems=errorList
  backLinkUrl=springUrl(backLinkUrl)>

  <#list plannedTenderDetailsList as listItem>
    <@plannedTenderListItem listItem=listItem listId=listItem?index/>
  </#list>

  <@fdsForm.htmlForm>
    <@fdsRadio.radio
      radioItems=radioItems
      path="form.hasMorePlannedTenderActivities"
      labelText="Do you want to add another planned tender activity?"/>
    <@fdsAction.button buttonText="Save and continue"/>
  </@fdsForm.htmlForm>

</@defaultPage>


<#macro plannedTenderListItem listItem listId>
  <#assign cardActionsContent>
    <@fdsSummaryList.summaryListCardActionList>
      <@fdsSummaryList.summaryListCardActionItem
        itemUrl=springUrl(listItem.changeLinkUrl())
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
    <@plannedTenderActivity.plannedTenderActivityRows plannedTenderActivity=listItem/>

  </@fdsSummaryList.summaryListCard>
</#macro>
