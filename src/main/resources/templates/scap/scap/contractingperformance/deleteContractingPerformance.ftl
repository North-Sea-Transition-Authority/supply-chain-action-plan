<#include '../../layout/layout.ftl'>
<#import  'contractingPerformanceSummaryContent.ftl' as summaryContent>

<#assign pageTitle = "Are you sure you want to delete this actual tender activity?"/>

<@defaultPage
  htmlTitle=pageTitle
  pageHeading=pageTitle
  pageSize=PageSize.TWO_THIRDS_COLUMN
  backLinkUrl=springUrl(backLinkUrl)
>
  <@fdsSummaryList.summaryListCard
    summaryListId="actual-tender-activity"
    headingText="Actual tender activity"
  >
    <@summaryContent.contractingPerformanceSummaryContent summaryView=summaryView countryMap=countryMap />
  </@fdsSummaryList.summaryListCard>
  <@fdsForm.htmlForm>
    <@fdsAction.submitButtons
      primaryButtonText="Delete"
      secondaryLinkText="Cancel"
      linkSecondaryAction=true
      linkSecondaryActionUrl=springUrl(backLinkUrl)
      primaryButtonClass="govuk-button govuk-button--warning"
    />
  </@fdsForm.htmlForm>
</@defaultPage>
