<#include '../layout/layout.ftl'>
<#import 'summary/scapSummary.ftl' as summary>

<#assign pageTitle = "Are you sure you want to delete this SCAP?" />
<@defaultPage
htmlTitle=pageTitle
pageHeading=pageTitle
pageSize=PageSize.TWO_THIRDS_COLUMN
backLinkUrl=springUrl(backLinkUrl)
caption=reference!""
>
  <@summary.summary scapSummaryView=scapSummaryView />

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
