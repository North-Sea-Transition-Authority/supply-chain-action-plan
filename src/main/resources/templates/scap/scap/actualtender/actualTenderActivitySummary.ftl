<#include '../../layout/layout.ftl'>
<#import  'actualTenderActivitySummaryContent.ftl' as summaryContent>

<#assign pageTitle = "Actual tender activity"/>

<#-- @ftlvariable name="errorList" type="java.util.List<uk.co.nstauthority.scap.fds.ErrorItem>" -->

<@defaultPage
htmlTitle=pageTitle
pageHeading=pageTitle
pageSize=PageSize.TWO_THIRDS_COLUMN
errorItems=errorList
backLinkUrl=springUrl(backLinkUrl)
>
  <#list actualTenderActivities as actualTenderActivity>
    <@actualTenderActivitySummary actualTenderActivity=actualTenderActivity index=actualTenderActivity_index/>
  </#list>

  <@fdsForm.htmlForm>
    <@fdsRadio.radio
      path="form.hasMoreActualTenderActivities"
      radioItems=radioItems
      labelText="Do you want to add another actual tendering activity?"
    />
    <@fdsAction.button buttonText="Save and complete"/>
  </@fdsForm.htmlForm>
</@defaultPage>


<#macro actualTenderActivitySummary actualTenderActivity index>
<#-- @ftlvariable name="actualTenderActivity"
  type="uk.co.nstauthority.scap.scap.actualtender.summary.ActualTenderSummaryView"
-->
  <#assign cardActionsContent>
    <@fdsSummaryList.summaryListCardActionList>
      <@fdsSummaryList.summaryListCardActionItem
        itemUrl=springUrl(actualTenderActivity.getChangeLinkUrl())
        itemText="Change"
        itemScreenReaderText="Change actual tendering activity ${index +1}"
      />
      <@fdsSummaryList.summaryListCardActionItem
        itemUrl=springUrl(actualTenderActivity.getDeleteLinkUrl())
        itemText="Delete"
        itemScreenReaderText="Delete actual tendering activity ${index +1}"
      />
    </@fdsSummaryList.summaryListCardActionList>
  </#assign>

  <@fdsSummaryList.summaryListCard
    summaryListId="actual-tender-activity-${index}"
    headingText="Actual tender activity ${index + 1}"
    cardActionsContent=cardActionsContent
  >
    <@summaryContent.actualTenderActivitySummaryContent actualTenderActivity=actualTenderActivity/>
  </@fdsSummaryList.summaryListCard>
</#macro>
