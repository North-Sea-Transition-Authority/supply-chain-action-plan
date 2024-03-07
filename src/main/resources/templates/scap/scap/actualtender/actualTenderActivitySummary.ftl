<#include '../../layout/layout.ftl'>
<#import  'actualTenderActivitySummaryContent.ftl' as summaryContent>
<#import 'actualTenderSummaryError.ftl' as errorMsg>

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

  <@fdsDetails.summaryDetails summaryTitle="What does \"Not from portal\" mean?">
    <p class="govuk-body">
      "Not from portal" means that this organisation has been manually added to this SCAP.<br>
    </p>
    <p class="govuk-body">
      If the organisation is added to the energy portal in the future, you may be asked to remove these manually added
      organisations and replace them with ones from the portal.
    </p>
  </@fdsDetails.summaryDetails>

  <@fdsForm.htmlForm>
    <@fdsRadio.radio
      path="form.hasMoreActualTenderActivities"
      radioItems=radioItems
      labelText="Do you want to add another actual tender activity?"
    />
    <@fdsAction.button buttonText="Save and complete"/>
  </@fdsForm.htmlForm>
</@defaultPage>


<#macro actualTenderActivitySummary actualTenderActivity index>
<#-- @ftlvariable name="actualTenderActivity"
  type="uk.co.nstauthority.scap.scap.summary.actualtender.ActualTenderActivitySummaryView"
-->
  <#assign cardActionsContent>
    <@fdsSummaryList.summaryListCardActionList>
      <@fdsSummaryList.summaryListCardActionItem
        itemUrl=springUrl(actualTenderActivity.getChangeLinkUrl())
        itemText="Change"
        itemScreenReaderText="Change actual tender activity ${index +1}"
      />
      <@fdsSummaryList.summaryListCardActionItem
        itemUrl=springUrl(actualTenderActivity.getDeleteLinkUrl())
        itemText="Delete"
        itemScreenReaderText="Delete actual tender activity ${index +1}"
      />
    </@fdsSummaryList.summaryListCardActionList>
  </#assign>

  <@fdsSummaryList.summaryListCard
    summaryListId="actual-tender-activity-${index}"
    headingText="Actual tender activity ${index + 1}"
    cardActionsContent=cardActionsContent
    summaryListErrorMessage=errorMsg.actualTenderSummaryErrorMessage(actualTenderActivity)
  >
    <@summaryContent.actualTenderActivitySummaryContent actualTenderActivity=actualTenderActivity/>
  </@fdsSummaryList.summaryListCard>
</#macro>
