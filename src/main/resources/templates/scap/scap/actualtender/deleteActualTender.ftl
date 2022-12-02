<#include '../../layout/layout.ftl'>
<#import  'actualTenderActivitySummaryContent.ftl' as summaryContent>

<#assign pageTitle = "Are you sure you want to delete this actual tender activity?"/>
<#-- @ftlvariable name="actualTenderActivityView"
type="uk.co.nstauthority.scap.scap.actualtender.summary.ActualTenderSummaryView"
-->

<@defaultPage
  htmlTitle=pageTitle
  pageHeading=pageTitle
  pageSize=PageSize.TWO_THIRDS_COLUMN
  backLinkUrl=springUrl(backLinkUrl)
>
  <@fdsSummaryList.summaryListWrapper
    summaryListId="actual-tender-activity"
  >
    <@fdsSummaryList.summaryList>
      <@summaryContent.actualTenderActivitySummaryContent actualTenderActivity=actualTenderActivityView />
    </@fdsSummaryList.summaryList>
  </@fdsSummaryList.summaryListWrapper>

  <#if contractingPerformanceWarning?has_content>
    <@fdsWarning.warning>
      ${contractingPerformanceWarning}
    </@fdsWarning.warning>
  </#if>

  <@fdsForm.htmlForm>
    <@fdsAction.submitButtons primaryButtonText="Delete" secondaryLinkText="Cancel" linkSecondaryAction=true linkSecondaryActionUrl=springUrl(backLinkUrl) primaryButtonClass="govuk-button govuk-button--warning"/>
  </@fdsForm.htmlForm>
</@defaultPage>