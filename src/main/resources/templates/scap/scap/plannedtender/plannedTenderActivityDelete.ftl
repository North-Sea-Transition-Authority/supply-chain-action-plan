<#include '../../layout/layout.ftl'>
<#import 'plannedTenderActivity.ftl' as plannedTenderActivity>

<#assign pageTitle = "Are you sure you want to delete this planned tender activity?"/>

<@defaultPage
htmlTitle=pageTitle
pageHeading=pageTitle
pageSize=PageSize.TWO_THIRDS_COLUMN
backLinkUrl=springUrl(backLinkUrl)
>
  <@fdsSummaryList.summaryListWrapper
    summaryListId="plannedTenderActivityToDelete"
    headingText="Planned tender activity"
  >
    <@fdsSummaryList.summaryList>
      <@plannedTenderActivity.plannedTenderActivityRows plannedTenderActivity=plannedTenderActivityView/>
    </@fdsSummaryList.summaryList>
  </@fdsSummaryList.summaryListWrapper>

  <@fdsForm.htmlForm actionUrl=springUrl(submitPostUrl)>
    <@fdsAction.submitButtons
      primaryButtonText="Delete"
      primaryButtonClass="govuk-button govuk-button--warning"
      secondaryLinkText="Cancel"
      linkSecondaryAction=true
      linkSecondaryActionUrl=springUrl(backLinkUrl)/>
  </@fdsForm.htmlForm>
</@defaultPage>
