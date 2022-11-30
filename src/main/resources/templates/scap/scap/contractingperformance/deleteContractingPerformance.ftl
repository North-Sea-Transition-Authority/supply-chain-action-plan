<#include '../../layout/layout.ftl'>
<#import  'contractingPerformanceSummaryContent.ftl' as summaryContent>

<#assign pageTitle = "Are you sure you want to delete this contracting performance?"/>

<@defaultPage
  htmlTitle=pageTitle
  pageHeading=pageTitle
  pageSize=PageSize.TWO_THIRDS_COLUMN
  backLinkUrl=springUrl(backLinkUrl)
>
  <@fdsSummaryList.summaryListWrapper
    summaryListId="contracting-performance"
    headingText="Contracting performance"
  >
    <@fdsSummaryList.summaryList>
      <@summaryContent.contractingPerformanceSummaryContent summaryView=summaryView countryMap=countryMap />
    </@fdsSummaryList.summaryList>
  </@fdsSummaryList.summaryListWrapper>
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
